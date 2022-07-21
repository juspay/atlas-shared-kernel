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

Module      :  Beckn.Mock.Utils
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Mock.Utils (module Beckn.Mock.Utils, maybeToEither) where

import Beckn.Types.Core.Error
import Data.Aeson hiding (Error)
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Types as Ae
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either.Extra
import Data.List
import Data.String.Conversions
import qualified Data.Text as T
import Data.Time
import Relude
import System.Random

-- | Read formatted time.
-- Here %F means the same as %Y-%m-%d, and %R acts like %H:%M.
-- Example: readUTCTime "2021-12-01 18:00"
readUTCTime :: Text -> Maybe UTCTime
readUTCTime = parseTimeM True defaultTimeLocale "%F %R" . T.unpack

textToError :: Text -> Error
textToError desc =
  Error
    { _type = CORE_ERROR,
      code = "400",
      path = Nothing,
      message = Just desc
    }

generateOrderId :: (MonadIO m) => m Text
generateOrderId = fmap show $ liftIO $ randomRIO (1000000, 9999999 :: Int)

whenRight :: Applicative m => Either e a -> (a -> m ()) -> m ()
whenRight eith f = either (\_ -> pure ()) f eith

encodeJSON :: (ToJSON a) => a -> BSL.ByteString
encodeJSON = Ae.encode . toJSON

decodeJSON :: (FromJSON a) => BS.ByteString -> Maybe a
decodeJSON bs = Ae.decode (BSL.fromStrict bs) >>= Ae.parseMaybe parseJSON

decodingErrorMessage :: BS.ByteString -> Text
decodingErrorMessage bs = "failed to decode JSON: " <> cs bs

decodeEitherJSON :: (FromJSON a) => BS.ByteString -> Either Text a
decodeEitherJSON bs = do
  val <- maybeToEither (decodingErrorMessage bs) (Ae.decode (BSL.fromStrict bs))
  first T.pack $ Ae.parseEither parseJSON val

findAndDecode :: (FromJSON a) => BS.ByteString -> [(BS.ByteString, BS.ByteString)] -> Either Text a
findAndDecode key list = maybeToEither errMsg (lookup key list) >>= decodeEitherJSON
  where
    errMsg = "failed to find key: " <> cs key
