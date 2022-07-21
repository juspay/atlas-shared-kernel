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

Module      :  Beckn.External.GoogleMaps.Client
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.External.GoogleMaps.Client where

import qualified Beckn.External.GoogleMaps.API as API
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common
import EulerHS.Prelude
import Servant.Client.Core (ClientError)

autoComplete ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Integer ->
  Text ->
  Text ->
  m GoogleMaps.SearchLocationResp
autoComplete url apiKey input location radius components lang = do
  callAPI url (API.autoComplete apiKey input location radius components lang) "autoComplete"
    >>= checkGoogleMapsError url

placeDetails ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  m GoogleMaps.PlaceDetailsResp
placeDetails url apiKey placeId fields = do
  callAPI url (API.placeDetails apiKey placeId fields) "placeDetails"
    >>= checkGoogleMapsError url

getPlaceName ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  m GoogleMaps.GetPlaceNameResp
getPlaceName url latLng apiKey = do
  callAPI url (API.getPlaceName latLng apiKey) "getPlaceName"
    >>= checkGoogleMapsError url

distanceMatrix ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  [GoogleMaps.Place] ->
  [GoogleMaps.Place] ->
  Text ->
  Maybe GoogleMaps.DepartureTime ->
  Maybe GoogleMaps.Mode ->
  m GoogleMaps.DistanceMatrixResp
distanceMatrix url origins destinations key departureTime mode = do
  callAPI url (API.distanceMatrix origins destinations key departureTime mode) "distanceMatrix"
    >>= checkGoogleMapsError url

directions ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  GoogleMaps.Place ->
  GoogleMaps.Place ->
  Text ->
  Maybe GoogleMaps.Mode ->
  Maybe [GoogleMaps.Place] ->
  m GoogleMaps.DirectionsResp
directions url origin destination key mode waypoints = do
  callAPI url (API.directions origin destination key (Just True) mode waypoints) "directionsAPI"
    >>= checkGoogleMapsError url

checkGoogleMapsError :: (MonadThrow m, Log m, HasField "status" a Text) => BaseUrl -> Either ClientError a -> m a
checkGoogleMapsError url res =
  fromEitherM (googleMapsError url) res >>= validateResponseStatus

googleMapsError :: BaseUrl -> ClientError -> ExternalAPICallError
googleMapsError = ExternalAPICallError (Just "GOOGLE_MAPS_API_ERROR")

validateResponseStatus :: (MonadThrow m, Log m, HasField "status" a Text) => a -> m a
validateResponseStatus response =
  case response.status of
    "OK" -> pure response
    "ZERO_RESULTS" -> pure response
    "INVALID_REQUEST" -> throwError GoogleMapsInvalidRequest
    _ -> throwError $ GoogleMapsCallError response.status
