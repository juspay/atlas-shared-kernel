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

Module      :  Beckn.Types.Core.Taxi.Search

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Taxi.Search
  ( module Beckn.Types.Core.Taxi.Search,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Search.Address as Reexport
import Beckn.Types.Core.Taxi.Search.Intent as Reexport
import Beckn.Types.Core.Taxi.Search.Location as Reexport
import Beckn.Types.Core.Taxi.Search.StartInfo as Reexport
import Beckn.Types.Core.Taxi.Search.StopInfo as Reexport
import Beckn.Types.Core.Taxi.Search.Time as Reexport
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype SearchMessage = SearchMessage
  { intent :: Intent
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

instance Example SearchMessage where
  example =
    SearchMessage
      { intent = example
      }
