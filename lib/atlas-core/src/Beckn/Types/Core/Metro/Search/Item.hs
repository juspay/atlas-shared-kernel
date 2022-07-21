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

Module      :  Beckn.Types.Core.Metro.Search.Item
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Metro.Search.Item where

import Beckn.Types.Core.Metro.Search.Descriptor (Descriptor)
import Beckn.Types.Core.Metro.Search.Price (Price)
import Beckn.Types.Core.Metro.Search.Tags (Tags)
import Beckn.Types.Core.Metro.Search.Time (Time)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data Item = Item
  { id :: Maybe Text,
    parent_item_id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    price :: Maybe Price,
    category_id :: Maybe Text,
    location_id :: Maybe Text,
    time :: Maybe Time,
    matched :: Maybe Bool,
    related :: Maybe Bool,
    recommended :: Maybe Bool,
    tags :: Maybe Tags
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
