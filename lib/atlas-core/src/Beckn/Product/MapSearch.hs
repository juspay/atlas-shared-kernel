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

Module      :  Beckn.Product.MapSearch
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Product.MapSearch where

import Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Prelude
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Common hiding (id)

getDistance ::
  ( MonadFlow m,
    CoreMetrics m,
    HasGoogleMaps m r
  ) =>
  Maybe MapSearch.TravelMode ->
  MapSearch.LatLong ->
  MapSearch.LatLong ->
  m GoogleMaps.GetDistanceResult
getDistance travelMode origin destination = do
  key <- asks (.googleMapsKey)
  case key of
    "mock-key" -> pure $ makeMockGetDistanceResult origin destination
    _ -> GoogleMaps.getDistance travelMode origin destination Nothing

getDistances ::
  ( MonadFlow m,
    CoreMetrics m,
    HasGoogleMaps m r
  ) =>
  Maybe MapSearch.TravelMode ->
  NonEmpty MapSearch.LatLong ->
  NonEmpty MapSearch.LatLong ->
  m [GoogleMaps.GetDistanceResult]
getDistances travelMode origins destinations = do
  key <- asks (.googleMapsKey)
  case key of
    "mock-key" -> pure $ makeMockGetDistanceResult <$> toList origins <*> toList destinations
    _ -> GoogleMaps.getDistances travelMode origins destinations Nothing

-- FIXME Should we use some calculation here?

makeMockGetDistanceResult ::
  MapSearch.LatLong ->
  MapSearch.LatLong ->
  GoogleMaps.GetDistanceResult
makeMockGetDistanceResult origin dest =
  GoogleMaps.GetDistanceResult
    { origin = GoogleMaps.Location $ LocationS origin.lat origin.lon,
      destination = GoogleMaps.Location $ LocationS dest.lat dest.lon,
      info =
        GoogleMaps.GetDistanceResultInfo
          { distance = Meter 9446,
            duration = mockDuration,
            duration_in_traffic = mockDuration,
            status = "OK"
          }
    }
  where
    mockDuration = intToNominalDiffTime 648
