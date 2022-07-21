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

Module      :  Beckn.Types.Core.Migration.Location
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Migration.Location where

import Beckn.Types.Core.Migration.Address (Address)
import Beckn.Types.Core.Migration.Circle (Circle)
import Beckn.Types.Core.Migration.City (City)
import Beckn.Types.Core.Migration.Country (Country)
import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Gps (Gps)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data Location = Location
  { id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    gps :: Maybe Gps,
    address :: Maybe Address,
    station_code :: Maybe Text,
    city :: Maybe City,
    country :: Maybe Country,
    circle :: Maybe Circle,
    polygon :: Maybe Text,
    _3dspace :: Maybe Text,
    time :: Maybe Time
  }
  deriving (Generic, Show, ToSchema)

emptyLocation :: Location
emptyLocation =
  Location
    { id = Nothing,
      descriptor = Nothing,
      gps = Nothing,
      address = Nothing,
      station_code = Nothing,
      city = Nothing,
      country = Nothing,
      circle = Nothing,
      polygon = Nothing,
      _3dspace = Nothing,
      time = Nothing
    }

instance Example Location where
  example =
    Location
      { id = Nothing,
        descriptor = Nothing,
        gps = Nothing,
        address = Nothing,
        station_code = Nothing,
        city = Nothing,
        country = Nothing,
        circle = Nothing,
        polygon = Nothing,
        _3dspace = Nothing,
        time = Nothing
      }

instance FromJSON Location where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Location where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
