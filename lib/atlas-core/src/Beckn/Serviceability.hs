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

Module      :  Beckn.Serviceability
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Serviceability where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import Beckn.Types.Geofencing
import Beckn.Types.MapSearch

rideServiceable ::
  (EsqDBFlow m r, HasField "geofencingConfig" r GeofencingConfig) =>
  (LatLong -> [Text] -> m Bool) ->
  LatLong ->
  Maybe LatLong ->
  m Bool
rideServiceable someGeometriesContain origin mbDestination = do
  geofencingConfig <- asks (.geofencingConfig)
  originServiceable <-
    case geofencingConfig.origin of
      Unrestricted -> pure True
      Regions regions -> someGeometriesContain origin regions
  destinationServiceable <-
    case geofencingConfig.destination of
      Unrestricted -> pure True
      Regions regions -> do
        maybe (pure True) (`someGeometriesContain` regions) mbDestination
  pure $ originServiceable && destinationServiceable
