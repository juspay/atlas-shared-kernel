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

Module      :  Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideBookingCancelledEvent

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideBookingCancelledEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideBookingCancelledEvent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.CancellationSource as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (RIDE_BOOKING_CANCELLED))
import Beckn.Utils.Example
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, name)
import EulerHS.Prelude hiding (id, (.=))
import GHC.Exts (fromList)

data RideBookingCancelledEvent = RideBookingCancelledEvent
  { order_id :: Text,
    cancellation_reason_id :: CancellationSource
  }
  deriving (Generic, Show)

instance ToJSON RideBookingCancelledEvent where
  toJSON RideBookingCancelledEvent {..} =
    A.Object $
      "order_id" .= order_id
        <> "cancellation_reason_id" .= cancellation_reason_id
        <> "update_type" .= RIDE_BOOKING_CANCELLED

instance FromJSON RideBookingCancelledEvent where
  parseJSON = withObject "RideBookingCancelledEvent" $ \obj -> do
    update_type <- obj .: "update_type"
    unless (update_type == RIDE_BOOKING_CANCELLED) $ fail "Wrong update_type."
    RideBookingCancelledEvent
      <$> obj .: "order_id"
      <*> obj .: "cancellation_reason_id"

instance ToSchema RideBookingCancelledEvent where
  declareNamedSchema _ = do
    id <- declareSchemaRef (Proxy :: Proxy Text)
    cancellationSource <- declareSchemaRef (Proxy :: Proxy CancellationSource)
    update_type <- declareSchemaRef (Proxy :: Proxy OnUpdateEventType)
    return $
      NamedSchema (Just "RideBookingCancelledEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("order_id", id),
                ("cancellation_reason_id", cancellationSource),
                ("update_type", update_type)
              ]
          & required
            L..~ [ "order_id",
                   "cancellation_reason_id",
                   "update_type"
                 ]

instance Example RideBookingCancelledEvent where
  example =
    RideBookingCancelledEvent
      { order_id = "ride_booking_id",
        cancellation_reason_id = ByDriver
      }
