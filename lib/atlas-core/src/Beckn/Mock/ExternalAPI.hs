{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}


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

Module      :  Beckn.Mock.ExternalAPI
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Mock.ExternalAPI where

import Beckn.Mock.App
import Beckn.Prelude (lookup)
import Beckn.Types.Common
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Error (GenericError (InternalError))
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.SignatureAuth (AuthenticatingEntity (getSignatureExpiry, getSigningKey))
import qualified Beckn.Utils.SignatureAuth as HttpSig
import Control.Monad
import qualified Control.Monad.Catch as C
import qualified Data.CaseInsensitive as CI
import Data.String.Conversions
import Data.Time.Clock.POSIX (getPOSIXTime)
import Fmt
import GHC.Records.Extra
import Network.HTTP.Client hiding (Proxy)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import Relude
import Servant.Client

callBapAPI ::
  forall api a b e.
  ( Show a,
    HasField "selfId" e Text,
    HasField "uniqueKeyId" e Text,
    HasClient ClientM api,
    Client ClientM api ~ (BecknCallbackReq a -> ClientM b),
    HasLog e,
    HasField "authManager" e Manager
  ) =>
  BecknCallbackReq a ->
  MockM e ()
callBapAPI req = do
  let bapUrl = req.context.bap_uri
  logOutput INFO "calling BAP"
  callAPI @api bapUrl req

callAPI ::
  forall api a b e.
  ( Show a,
    HasField "selfId" e Text,
    HasField "uniqueKeyId" e Text,
    HasClient ClientM api,
    Client ClientM api ~ (BecknCallbackReq a -> ClientM b),
    HasLog e,
    HasField "authManager" e Manager
  ) =>
  BaseUrl ->
  BecknCallbackReq a ->
  MockM e ()
callAPI url req = do
  let clientFunc = client @api Proxy
      clientAction = clientFunc req
  logOutput INFO $ mconcat ["calling ", show req.context.action, "; url=", show url]
  logOutput DEBUG $ show req
  _ <- callClientM url clientAction
  pure ()

callClientM ::
  ( HasField "selfId" e Text,
    HasField "uniqueKeyId" e Text,
    HasField "authManager" e Manager
  ) =>
  BaseUrl ->
  ClientM a ->
  MockM e a
callClientM url clientAction = do
  manager <- asks (.authManager)
  res <- liftIO $ runClientM clientAction $ mkClientEnv manager url
  either C.throwM pure res

prepareAuthManager ::
  AuthenticatingEntity cfg =>
  cfg ->
  [Text] ->
  Text ->
  Text ->
  (LogLevel -> Text -> IO ()) ->
  Http.ManagerSettings
prepareAuthManager appCfg signHeaders subscriberId uniqueKeyId logger =
  Http.tlsManagerSettings {Http.managerModifyRequest = doSignature}
  where
    doSignature :: Request -> IO Request
    doSignature req = do
      now <- getPOSIXTime
      let params = HttpSig.mkSignatureParams subscriberId uniqueKeyId now signatureExpiry HttpSig.Ed25519
      let body = getBody $ Http.requestBody req
      let bodyHash = HttpSig.becknSignatureHash body
      let headers = Http.requestHeaders req
      let signatureMsg = HttpSig.makeSignatureString params bodyHash headers
      logger DEBUG $ "Request body for signing: " +|| body ||+ ""
      logger DEBUG $ "Signature Message: " +|| signatureMsg ||+ ""
      let mbRes = foldM (addSignature bodyHash params headers) req signHeaders
      maybe (C.throwM $ InternalError $ "Could not add signature: " <> show params) pure mbRes
    getBody (Http.RequestBodyLBS body) = cs body
    getBody (Http.RequestBodyBS body) = body
    getBody _ = "<MISSING_BODY>"
    signPrivKey = getSigningKey appCfg
    signatureExpiry = getSignatureExpiry appCfg

    -- FIXME: we don't currently deal with Content-Length not being there (this is
    -- filled later, so we might need to have some special handling)
    addSignature bodyHash params headers req signHeader =
      let ciHeader = CI.mk $ encodeUtf8 signHeader
       in -- We check if the signHeader exists because `managerModifyRequest` might be
          -- called multiple times, so we already added it once, let's skip right over
          if isJust $ lookup ciHeader headers
            then Just req
            else do
              signature <- HttpSig.sign signPrivKey params bodyHash headers
              let headerVal = HttpSig.encode $ HttpSig.SignaturePayload signature params
              Just $ req {Http.requestHeaders = (ciHeader, headerVal) : headers}
