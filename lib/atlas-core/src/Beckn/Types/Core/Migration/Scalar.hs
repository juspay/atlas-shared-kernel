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

Module      :  Beckn.Types.Core.Migration.Scalar
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Migration.Scalar (Scalar (..), Range (..)) where

import Beckn.Utils.JSON
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (max, min)

data Scalar = Scalar
  { _type :: Maybe ScalarType,
    value :: Int, -- FIXME: probably not integer
    estimated_value :: Maybe Int,
    computed_value :: Maybe Int,
    range :: Maybe Range,
    unit :: Text
  }
  deriving (Generic, Show, ToSchema)

data ScalarType = CONSTANT | VARIABLE
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data Range = Range
  { min :: Int,
    max :: Int
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance FromJSON Scalar where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Scalar where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
