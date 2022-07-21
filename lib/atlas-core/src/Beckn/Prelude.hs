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

Module      :  Beckn.Prelude
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Prelude (module E, module Beckn.Prelude) where

import Control.Arrow as E
import qualified Control.Concurrent as Conc
import Control.Concurrent.STM.TMVar as E (TMVar)
import Control.Exception as E (SomeException)
import Control.Exception.Safe as E (try)
import Control.Monad.Catch as E
  ( Exception (..),
    MonadCatch (..),
    MonadThrow (..),
    SomeException (..),
  )
import Control.Monad.Reader as E
import Data.Aeson as E (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Bool as E (bool)
import Data.Fixed
import Data.Foldable as E
import Data.Function as E hiding (id)
import Data.Functor as E
import Data.Functor.Identity as E
import Data.Kind as E (Type)
import Data.List.NonEmpty as E (NonEmpty (..))
import Data.Maybe as E (fromMaybe, listToMaybe)
import Data.OpenApi as E (ToParamSchema, ToSchema)
import Data.Proxy as E (Proxy (..))
import Data.String as E (IsString (..))
import Data.Text as E (Text)
import qualified Data.Text as T
import Data.Time as E (TimeOfDay)
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock as E (NominalDiffTime, UTCTime)
import GHC.Generics as E (Generic, Generic1)
import GHC.Int as E (Int64)
import GHC.Records.Compat as E
import GHC.Stack as E (HasCallStack)
import Servant.Client as E (BaseUrl)
import qualified Servant.Client as Servant
import Universum.Debug as E
import Universum.Print as E
import Universum.String.Conversion as E
import Prelude as E hiding (error, id, log, print, putStr, putStrLn, show, undefined)

foldWIndex :: (Integer -> acc -> a -> acc) -> acc -> [a] -> acc
foldWIndex f acc p = snd $ foldl' (\(i, acc') c -> (i + 1, f i acc' c)) (0, acc) p

identity :: p -> p
identity a = a

everyPossibleVariant :: (Enum a, Bounded a) => [a]
everyPossibleVariant = [minBound .. maxBound]

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb thing = do
  b <- mb
  when b thing

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb = whenM (not <$> mb)

showBaseUrl :: BaseUrl -> Text
showBaseUrl = T.pack . Servant.showBaseUrl

parseBaseUrl :: MonadThrow m => Text -> m BaseUrl
parseBaseUrl = Servant.parseBaseUrl . T.unpack

whileM :: Monad m => m Bool -> m () -> m ()
whileM b f = whenM b $ f >> whileM b f

threadDelay :: MonadIO m => Int -> m ()
threadDelay = liftIO . Conc.threadDelay

rightToMaybe :: Either e a -> Maybe a
rightToMaybe = either (const Nothing) Just

intToNominalDiffTime :: Int -> NominalDiffTime
intToNominalDiffTime = secondsToNominalDiffTime . MkFixed . (* 1000000000000) . toInteger
