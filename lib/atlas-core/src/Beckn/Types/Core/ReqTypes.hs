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

Module      :  Beckn.Types.Core.ReqTypes
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.ReqTypes where

import Beckn.Types.Core.Context (Context)
import Beckn.Types.Core.Error (Error)
import Beckn.Utils.GenericPretty
import qualified Control.Lens as L
import Data.Aeson
import Data.OpenApi
import Data.Typeable
import EulerHS.Prelude hiding ((.=), (.~))
import GHC.Exts (IsList (fromList))

data BecknReq a = BecknReq
  { context :: Context,
    message :: a
  }
  deriving (Generic, Show, FromJSON, ToJSON, PrettyShow)

instance ToSchema a => ToSchema (BecknReq a)

data BecknCallbackReq a = BecknCallbackReq
  { context :: Context,
    contents :: Either Error a
  }
  deriving (Generic, Show, PrettyShow)

instance (ToSchema a) => ToSchema (BecknCallbackReq a) where
  declareNamedSchema _ = do
    context <- declareSchemaRef (Proxy :: Proxy Context)
    err <- declareSchemaRef (Proxy :: Proxy Error)
    let messageTypeName = show $ typeRep (Proxy :: Proxy a)
    message <- declareSchemaRef (Proxy :: Proxy a)
    let errVariant =
          Inline $
            mempty
              & type_ L.?~ OpenApiObject
              & properties L..~ fromList [("context", context), ("error", err)]
              & required L..~ ["context", "error"]
        messageVariant =
          Inline $
            mempty
              & type_ L.?~ OpenApiObject
              & properties L..~ fromList [("context", context), ("message", message)]
              & required L..~ ["context", "message"]
    return $
      NamedSchema (Just $ "BecknCallbackReq_" <> messageTypeName) $
        mempty
          & type_ L.?~ OpenApiObject
          & oneOf L.?~ [messageVariant, errVariant]

instance ToJSON a => ToJSON (BecknCallbackReq a) where
  toJSON (BecknCallbackReq context contents) = object $ contextField : errorOrMessage
    where
      contextField = "context" .= context
      errorOrMessage = case contents of
        Left err -> ["error" .= err]
        Right message -> ["message" .= message]

instance FromJSON a => FromJSON (BecknCallbackReq a) where
  parseJSON = withObject "BecknCallbackReq" $ \o ->
    BecknCallbackReq
      <$> o .: "context"
      <*> (Left <$> o .: "error" <|> Right <$> o .: "message")
