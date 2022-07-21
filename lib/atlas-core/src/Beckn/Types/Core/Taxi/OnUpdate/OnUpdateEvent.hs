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

Module      :  Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideAssignedEvent as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideBookingCancelledEvent as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideBookingReallocationEvent as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideStartedEvent as Reexport
import qualified Beckn.Utils.JSON as J
import qualified Beckn.Utils.Schema as S
import Data.OpenApi
import EulerHS.Prelude hiding ((.=))

data OnUpdateEvent
  = RideAssigned RideAssignedEvent
  | RideStarted RideStartedEvent
  | RideCompleted RideCompletedEvent
  | RideBookingCancelled RideBookingCancelledEvent
  | RideBookingReallocation RideBookingReallocationEvent
  deriving (Generic, Show)

instance ToJSON OnUpdateEvent where
  toJSON = genericToJSON J.untaggedValue

instance FromJSON OnUpdateEvent where
  parseJSON = genericParseJSON J.untaggedValue

instance ToSchema OnUpdateEvent where
  declareNamedSchema = genericDeclareNamedSchema S.untaggedValue
