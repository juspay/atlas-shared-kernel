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

Module      :  Beckn.Types.Core.Migration.Cancellation
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Migration.Cancellation (Cancellation (..)) where

import Beckn.Types.Common (IdObject)
import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Option (Option)
import Beckn.Types.Core.Migration.Policy (Policy)
import Beckn.Utils.JSON
import Data.Time (UTCTime)
import EulerHS.Prelude

data Cancellation = Cancellation
  { _type :: Maybe CancellationType,
    ref_id :: Maybe Text,
    policies :: Maybe [Policy],
    time :: Maybe UTCTime,
    cancelled_by :: Maybe Text,
    reasons :: Maybe Option,
    selected_reason :: Maybe IdObject,
    additional_description :: Maybe Descriptor
  }
  deriving (Generic, Show)

data CancellationType = FULL | PARTIAL
  deriving (Generic, Show)

instance FromJSON Cancellation where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Cancellation where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON CancellationType where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON CancellationType where
  toJSON = genericToJSON constructorsToLowerOptions
