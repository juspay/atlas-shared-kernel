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

Module      :  Beckn.Types.Core.Migration.Operator
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Migration.Operator where

import Beckn.Types.Core.Migration.Person (Person)
import Beckn.Utils.JSON (uniteObjects)
import EulerHS.Prelude

-- allOf case
data Operator = Operator Person Experience'

newtype Experience' = Experience' {experience :: Experience}
  deriving (Generic, Show, ToJSON, FromJSON)

data Experience = Experience
  { label :: Maybe Text,
    value :: Maybe Text,
    unit :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance FromJSON Operator where
  parseJSON v = Operator <$> parseJSON v <*> parseJSON v

instance ToJSON Operator where
  toJSON (Operator p e) = uniteObjects [toJSON p, toJSON e]
