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

Module      :  Beckn.Types.APISuccess
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.APISuccess (APISuccess (..)) where

import Data.Aeson hiding (Success)
import Data.Aeson.Types (parseFail, typeMismatch)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding ((.=))

data APISuccess = Success deriving (Generic, Show, Eq, ToSchema)

instance ToJSON APISuccess where
  toJSON Success = object ["result" .= ("Success" :: Text)]

instance FromJSON APISuccess where
  parseJSON (Object obj) = do
    result :: String <- obj .: "result"
    case result of
      "Success" -> pure Success
      _ -> parseFail "Expected \"Success\" in \"result\" field."
  parseJSON wrongVal = typeMismatch "Object APISuccess" wrongVal
