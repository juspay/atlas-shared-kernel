{- |
Copyright 2022 Juspay Technologies Pvt Ltd

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Module      :  Beckn.Utils.Monitoring.Prometheus.Servant
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Monitoring.Prometheus.Servant where

import Data.Proxy
import Data.Text as DT
import EulerHS.Prelude as E
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.Wai (Request (..))
import Servant

class SanitizedUrl a where
  getSanitizedUrl :: Proxy a -> Request -> Maybe Text

instance
  (SanitizedUrl (a :: Type), SanitizedUrl (b :: Type)) =>
  SanitizedUrl (a :<|> b)
  where
  getSanitizedUrl _ req =
    getSanitizedUrl (Proxy :: Proxy a) req
      <|> getSanitizedUrl (Proxy :: Proxy b) req

instance
  ( KnownSymbol (path :: Symbol),
    SanitizedUrl (subroute :: Type)
  ) =>
  SanitizedUrl (path :> subroute)
  where
  getSanitizedUrl _ req = do
    let path = pathInfo req
    if E.null path
      then Nothing
      else do
        let (x : xs) = path
            p = DT.pack $ symbolVal (Proxy :: Proxy path)
        if p == x
          then
            let maybeUrl = getSanitizedUrl (Proxy :: Proxy subroute) $ req {pathInfo = xs}
             in (\url -> Just (p <> "/" <> url)) =<< maybeUrl
          else Nothing

instance
  ( KnownSymbol (capture :: Symbol),
    SanitizedUrl (subroute :: Type)
  ) =>
  SanitizedUrl (Capture capture a :> subroute)
  where
  getSanitizedUrl _ req = do
    let path = pathInfo req
    if E.null path
      then Nothing
      else
        let (_ : xs) = path
            p = DT.pack $ ":" <> symbolVal (Proxy :: Proxy capture)
            maybeUrl = getSanitizedUrl (Proxy :: Proxy subroute) $ req {pathInfo = xs}
         in (\url -> Just (p <> "/" <> url)) =<< maybeUrl

instance
  ReflectMethod m =>
  SanitizedUrl (Verb (m :: StdMethod) code contentType a)
  where
  getSanitizedUrl _ req = do
    let p = pathInfo req
    if E.null p && requestMethod req == reflectMethod (Proxy :: Proxy m)
      then Just ""
      else Nothing

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (QueryParams (h :: Symbol) a :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (Header h a :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (ReqBody cts a :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (QueryParam' modifier name t :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (Header' '[Required, Strict] h v :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)
