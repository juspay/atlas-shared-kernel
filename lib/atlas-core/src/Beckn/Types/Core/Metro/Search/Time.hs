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

Module      :  Beckn.Types.Core.Metro.Search.Time
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Metro.Search.Time where

import Beckn.Types.Core.Metro.Search.Duration (Duration)
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import EulerHS.Prelude

data Time = Time
  { label :: Maybe Text,
    timestamp :: Maybe UTCTime,
    duration :: Maybe Duration,
    range :: Maybe TimeRange,
    days :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)

emptyTime :: Time
emptyTime =
  Time
    { label = Nothing,
      timestamp = Nothing,
      duration = Nothing,
      range = Nothing,
      days = Nothing
    }

data TimeRange = TimeRange
  { start :: UTCTime,
    end :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)
