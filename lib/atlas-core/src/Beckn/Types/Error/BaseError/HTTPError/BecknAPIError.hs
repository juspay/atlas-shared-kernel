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

Module      :  Beckn.Types.Error.BaseError.HTTPError.BecknAPIError

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Error.BaseError.HTTPError.BecknAPIError
  ( module Beckn.Types.Error.BaseError.HTTPError.BecknAPIError,
    Error.Error (..),
    Error.ErrorType (..),
  )
where

import qualified Beckn.Types.Core.Error as Error
import Beckn.Types.Error.BaseError.HTTPError.FromResponse
import Data.Aeson.Types
import EulerHS.Prelude hiding (Show, show, (.=))
import Prelude (Show (..))

class IsBecknAPIError e where
  toType :: e -> Error.ErrorType
  toType _ = Error.INTERNAL_ERROR

  toPath :: e -> Maybe Text
  toPath _ = Nothing

newtype BecknAPIError = BecknAPIError Error.Error
  deriving (Generic, Eq, Show)

instance FromJSON BecknAPIError where
  parseJSON (Object v) = BecknAPIError <$> v .: "error"
  parseJSON invalid =
    prependFailure
      "Parsing BecknAPIError failed, "
      (typeMismatch "Object" invalid)

instance ToJSON BecknAPIError where
  toJSON (BecknAPIError err) = object ["message" .= ack, "error" .= err]
    where
      ack = object ["ack" .= status]
      status = object ["status" .= ("NACK" :: Text)]

instance FromResponse BecknAPIError where
  fromResponse = fromJsonResponse
