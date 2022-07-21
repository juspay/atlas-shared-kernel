{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


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

Module      :  Beckn.Types.Error
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Error where

import Beckn.External.MyValueFirst.Types (SubmitSmsRes, submitSmsResToText)
import Beckn.Types.Error.BaseError
import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (fromResponse))
import Beckn.Utils.Servant.BaseUrl
import EulerHS.Prelude
import EulerHS.Types (KVDBReply)
import qualified Kafka.Types as Kafka
import Network.HTTP.Types (Header, Status (statusCode))
import Network.HTTP.Types.Header (HeaderName)
import Servant.Client (BaseUrl, ClientError, ResponseF (responseStatusCode))

-- TODO: sort out proper codes, namings and usages for Unauthorized and AccessDenied
data AuthError
  = Unauthorized
  | InvalidAuthData
  | TokenExpired
  | TokenIsNotVerified
  | TokenNotFound Text
  | InvalidToken Text
  | AuthBlocked Text
  | IncorrectOTP
  | AccessDenied
  | HitsLimitError Int
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''AuthError

instance IsBaseError AuthError where
  toMessage = \case
    TokenNotFound tokenId -> Just $ "Token with tokenId \"" <> show tokenId <> "\" not found."
    InvalidToken token -> Just $ "Invalid token: " <> token
    AuthBlocked reason -> Just $ "Authentication process blocked: " <> reason
    AccessDenied -> Just "You have no access to this operation."
    HitsLimitError hitsLimitResetTime -> Just $ "Hits limit reached. Try again in " <> show hitsLimitResetTime <> " sec."
    _ -> Nothing

instance IsHTTPError AuthError where
  toErrorCode = \case
    Unauthorized -> "UNAUTHORIZED"
    InvalidAuthData -> "INVALID_AUTH_DATA"
    TokenExpired -> "TOKEN_EXPIRED"
    TokenIsNotVerified -> "TOKEN_IS_NOT_VERIFIED"
    TokenNotFound _ -> "TOKEN_NOT_FOUND"
    InvalidToken _ -> "INVALID_TOKEN"
    AuthBlocked _ -> "AUTH_BLOCKED"
    IncorrectOTP -> "INCORRECT_OTP"
    AccessDenied -> "ACCESS_DENIED"
    HitsLimitError _ -> "HITS_LIMIT_EXCEED"
  toHttpCode = \case
    Unauthorized -> E401
    InvalidToken _ -> E401
    AccessDenied -> E403
    TokenIsNotVerified -> E403
    HitsLimitError _ -> E429
    _ -> E400

instance IsAPIError AuthError

data HeaderError
  = MissingHeader HeaderName
  | InvalidHeader HeaderName Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''HeaderError

instance IsBaseError HeaderError where
  toMessage = \case
    MissingHeader headerName -> Just $ "Header " +|| headerName ||+ " is missing"
    InvalidHeader headerName err -> Just $ "Header " +|| headerName ||+ " is invalid: " +|| err ||+ ""

instance IsHTTPError HeaderError where
  toErrorCode = \case
    MissingHeader _ -> "MISSING_HEADER"
    InvalidHeader _ _ -> "INVALID_HEADER"
  toHttpCode _ = E400

instance IsAPIError HeaderError

data SignatureError
  = SignatureVerificationFailure [Header]
  | CannotDecodeSignature String
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SignatureError

instance IsBaseError SignatureError where
  toMessage = \case
    CannotDecodeSignature err -> Just (fromString err)
    _ -> Nothing

instance IsHTTPError SignatureError where
  toErrorCode = \case
    SignatureVerificationFailure _ -> "SIGNATURE_VERIFICATION_FAILURE"
    CannotDecodeSignature _ -> "CANNOT_DECODE_SIGNATURE"
  toHttpCode _ = E401
  toCustomHeaders (SignatureVerificationFailure headers) = headers
  toCustomHeaders _ = []

instance IsAPIError SignatureError

data AuthPIError = NotAnExecutor deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''AuthPIError

instance IsBaseError AuthPIError where
  toMessage NotAnExecutor = Just "You are not an executor of this ride."

instance IsHTTPError AuthPIError where
  toErrorCode NotAnExecutor = "NOT_AN_EXECUTOR"
  toHttpCode NotAnExecutor = E403

instance IsAPIError AuthPIError

data VehicleError
  = VehicleNotFound Text
  | VehicleDoesNotExist Text
  | VehicleFieldNotPresent Text
  | VehicleAlreadyLinked
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''VehicleError

instance IsBaseError VehicleError where
  toMessage = \case
    VehicleNotFound vehicleId -> Just $ "Vehicle with vehicleId \"" <> show vehicleId <> "\" not found."
    VehicleDoesNotExist vehicleId -> Just $ "Vehicle with vehicleId \"" <> show vehicleId <> "\" not exist."
    VehicleFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this vehicle."
    _ -> Nothing

instance IsHTTPError VehicleError where
  toErrorCode = \case
    VehicleNotFound _ -> "VEHICLE_NOT_FOUND"
    VehicleDoesNotExist _ -> "VEHICLE_DOES_NOT_EXIST"
    VehicleFieldNotPresent _ -> "VEHICLE_FIELD_NOT_PRESENT"
    VehicleAlreadyLinked -> "VEHICLE_ALREADY_LINKED"
  toHttpCode = \case
    VehicleNotFound _ -> E500
    VehicleDoesNotExist _ -> E400
    VehicleFieldNotPresent _ -> E500
    VehicleAlreadyLinked -> E400

instance IsAPIError VehicleError

data PersonError
  = PersonNotFound Text
  | PersonDoesNotExist Text
  | PersonFieldNotPresent Text
  | PersonOrgExists
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''PersonError

instance IsBaseError PersonError where
  toMessage = \case
    PersonNotFound personId -> Just $ "Person with personId \"" <> show personId <> "\" not found."
    PersonDoesNotExist personId -> Just $ "No person matches passed data \"" <> show personId <> "\" not exist."
    PersonFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this person."
    PersonOrgExists -> Just "Person is already registered in the organization."

instance IsHTTPError PersonError where
  toErrorCode = \case
    PersonNotFound _ -> "PERSON_NOT_FOUND"
    PersonDoesNotExist _ -> "PERSON_DOES_NOT_EXIST"
    PersonFieldNotPresent _ -> "PERSON_FIELD_NOT_PRESENT"
    PersonOrgExists -> "PERSON_ORG_ALREADY_EXISTS"
  toHttpCode = \case
    PersonNotFound _ -> E500
    PersonDoesNotExist _ -> E400
    PersonFieldNotPresent _ -> E500
    PersonOrgExists -> E400

instance IsAPIError PersonError

data RegistryError
  = SubscriberNotFound
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RegistryError

instance IsBaseError RegistryError where
  toMessage = \case
    SubscriberNotFound -> Just "Couldn't find subscriber in registry."

instance IsHTTPError RegistryError where
  toErrorCode = \case
    SubscriberNotFound -> "SUBSCRIBER_NOT_FOUND"
  toHttpCode = \case
    SubscriberNotFound -> E500

instance IsAPIError RegistryError

data LocationError
  = LocationNotFound
  | LocationDoesNotExist
  | LocationFieldNotPresent Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''LocationError

instance IsBaseError LocationError where
  toMessage = \case
    LocationDoesNotExist -> Just "No location matches passed data."
    LocationFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this location."

instance IsHTTPError LocationError where
  toErrorCode = \case
    LocationNotFound -> "LOCATION_NOT_FOUND"
    LocationDoesNotExist -> "LOCATION_DOES_NOT_EXIST"
    LocationFieldNotPresent _ -> "LOCATION_FIELD_NOT_PRESENT"
  toHttpCode = \case
    LocationNotFound -> E500
    LocationDoesNotExist -> E400
    LocationFieldNotPresent _ -> E500

instance IsAPIError LocationError

data GenericError
  = InternalError Text
  | InvalidRequest Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''GenericError

instance IsBaseError GenericError where
  toMessage = \case
    InternalError msg -> Just msg
    InvalidRequest msg -> Just msg

instance IsHTTPError GenericError where
  toErrorCode = \case
    InternalError _ -> "INTERNAL_ERROR"
    InvalidRequest _ -> "INVALID_REQUEST"
  toHttpCode = \case
    InternalError _ -> E500
    InvalidRequest _ -> E400

instance IsAPIError GenericError

data OrganizationError
  = OrgNotFound Text
  | OrgDoesNotExist Text
  | OrgFieldNotPresent Text
  | OrgMobilePhoneUsed
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''OrganizationError

instance IsBaseError OrganizationError where
  toMessage = \case
    OrgNotFound orgId -> Just $ "Organization with orgId \"" <> show orgId <> "\" not found."
    OrgDoesNotExist orgId -> Just $ "No organization matches passed data \"" <> show orgId <> "\" not exist."
    OrgFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this organization."
    OrgMobilePhoneUsed -> Just "Mobile phone already used by another organization."

instance IsHTTPError OrganizationError where
  toErrorCode = \case
    OrgNotFound _ -> "ORGANIZATION_NOT_FOUND"
    OrgDoesNotExist _ -> "ORGANIZATION_DOES_NOT_EXIST"
    OrgFieldNotPresent _ -> "ORGANIZATION_FIELD_NOT_PRESENT"
    OrgMobilePhoneUsed -> "ORGANIZATION_MOBILE_PHONE_USED"
  toHttpCode = \case
    OrgDoesNotExist _ -> E400
    OrgMobilePhoneUsed -> E400
    _ -> E500

instance IsAPIError OrganizationError

data SearchRequestError
  = SearchRequestNotFound Text
  | SearchRequestDoesNotExist Text
  | SearchRequestExpired
  | SearchRequestInvalidStatus Text
  | SearchRequestFieldNotPresent Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SearchRequestError

instance IsBaseError SearchRequestError where
  toMessage = \case
    SearchRequestNotFound searchId -> Just $ "Search with searchId \"" <> show searchId <> "\"not found. "
    SearchRequestDoesNotExist searchId -> Just $ "No case matches passed data \"<>" <> show searchId <> "\" not exist"
    SearchRequestFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this case."
    SearchRequestInvalidStatus msg -> Just $ "Attempted to do some action in wrong case status. " <> msg
    _ -> Nothing

instance IsHTTPError SearchRequestError where
  toErrorCode = \case
    SearchRequestNotFound _ -> "SEARCH_REQUEST_NOT_FOUND"
    SearchRequestDoesNotExist _ -> "SEARCH_REQUEST_DOES_NOT_EXIST"
    SearchRequestExpired -> "SEARCH_REQUEST_EXPIRED"
    SearchRequestFieldNotPresent _ -> "SEARCH_REQUEST_FIELD_NOT_PRESENT"
    SearchRequestInvalidStatus _ -> "SEARCH_REQUEST_INVALID_STATUS"
  toHttpCode = \case
    SearchRequestNotFound _ -> E500
    SearchRequestDoesNotExist _ -> E400
    SearchRequestExpired -> E400
    SearchRequestFieldNotPresent _ -> E500
    SearchRequestInvalidStatus _ -> E400

instance IsAPIError SearchRequestError

data QuoteError
  = QuoteNotFound Text
  | QuoteDoesNotExist Text
  | QuoteFieldNotPresent Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''QuoteError

instance IsBaseError QuoteError where
  toMessage = \case
    QuoteNotFound quoteId -> Just $ "Quote with quoteId \"" <> show quoteId <> "\" not found. "
    QuoteDoesNotExist quoteId -> Just $ "No quote matches passed data \"" <> show quoteId <> "\" not exist. "
    QuoteFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this quote."

instance IsHTTPError QuoteError where
  toErrorCode = \case
    QuoteNotFound _ -> "QUOTE_NOT_FOUND"
    QuoteDoesNotExist _ -> "QUOTE_DOES_NOT_EXIST"
    QuoteFieldNotPresent _ -> "QUOTE_FIELD_NOT_PRESENT"
  toHttpCode = \case
    QuoteNotFound _ -> E500
    QuoteDoesNotExist _ -> E400
    QuoteFieldNotPresent _ -> E500

instance IsAPIError QuoteError

data RideBookingError
  = RideBookingNotFound Text
  | RideBookingDoesNotExist Text
  | RideBookingFieldNotPresent Text
  | RideBookingInvalidStatus Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RideBookingError

instance IsBaseError RideBookingError where
  toMessage = \case
    RideBookingNotFound bookingId -> Just $ "RideBooking with bookingId \"" <> show bookingId <> "\" not found. "
    RideBookingDoesNotExist bookingId -> Just $ "No ride booking matches passed data \"" <> show bookingId <> "\" not exist. "
    RideBookingFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this ride booking."
    RideBookingInvalidStatus msg -> Just $ "Attempted to do some action in wrong ride booking status. " <> msg

instance IsHTTPError RideBookingError where
  toErrorCode = \case
    RideBookingNotFound _ -> "RIDE_BOOKING_NOT_FOUND"
    RideBookingDoesNotExist _ -> "RIDE_BOOKING_DOES_NOT_EXIST"
    RideBookingFieldNotPresent _ -> "RIDE_BOOKING_FIELD_NOT_PRESENT"
    RideBookingInvalidStatus _ -> "RIDE_BOOKING_INVALID_STATUS"
  toHttpCode = \case
    RideBookingNotFound _ -> E500
    RideBookingDoesNotExist _ -> E400
    RideBookingFieldNotPresent _ -> E500
    RideBookingInvalidStatus _ -> E400

instance IsAPIError RideBookingError

data RideError
  = RideNotFound Text
  | RideDoesNotExist Text
  | RideFieldNotPresent Text
  | RideInvalidStatus Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RideError

instance IsBaseError RideError where
  toMessage = \case
    RideNotFound rideId -> Just $ "Ride with rideId \"" <> show rideId <> "\"not found. "
    RideDoesNotExist rideId -> Just $ "No ride matches passed data \"" <> show rideId <> "\" not exist. "
    RideFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this ride."
    RideInvalidStatus msg -> Just $ "Attempted to do some action in wrong ride status. " <> msg

instance IsHTTPError RideError where
  toErrorCode = \case
    RideNotFound _ -> "RIDE_NOT_FOUND"
    RideDoesNotExist _ -> "RIDE_DOES_NOT_EXIST"
    RideFieldNotPresent _ -> "RIDE_FIELD_NOT_PRESENT"
    RideInvalidStatus _ -> "RIDE_INVALID_STATUS"

  toHttpCode = \case
    RideNotFound _ -> E500
    RideDoesNotExist _ -> E400
    RideFieldNotPresent _ -> E500
    RideInvalidStatus _ -> E400

instance IsAPIError RideError

data RiderDetailsError
  = RiderDetailsNotFound Text
  | RiderDetailsDoesNotExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RiderDetailsError

instance IsBaseError RiderDetailsError where
  toMessage = \case
    RiderDetailsNotFound rideDetailId -> Just $ "RideDetails with rideDetailsId \"" <> show rideDetailId <> "\" not found. "
    RiderDetailsDoesNotExist rideDetailId -> Just $ "No rider details matches passed data \"" <> show rideDetailId <> "\" not exist. "

instance IsHTTPError RiderDetailsError where
  toErrorCode = \case
    RiderDetailsNotFound _ -> "RIDER_DETAILS_NOT_FOUND"
    RiderDetailsDoesNotExist _ -> "RIDER_DETAILS_DOES_NOT_EXIST"
  toHttpCode = \case
    RiderDetailsNotFound _ -> E500
    RiderDetailsDoesNotExist _ -> E400

instance IsAPIError RiderDetailsError

data GatewayError
  = UnsupportedGatewaySelector
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''GatewayError

instance IsBaseError GatewayError

instance IsHTTPError GatewayError where
  toErrorCode UnsupportedGatewaySelector = "UNSUPPORTED_GATEWAY_SELECTOR"

instance IsAPIError GatewayError

data DatabaseError
  = NotPostgresBackend
  | SQLRequestError Text Text
  | SQLResultError Text
  | DBUnknownError Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DatabaseError

instance IsBaseError DatabaseError where
  toMessage = \case
    SQLRequestError sqlErr desc -> Just $ "SQL request error: " <> sqlErr <> ". Description: " <> desc
    SQLResultError msg -> Just msg
    DBUnknownError msg -> Just msg
    _ -> Nothing

instance IsHTTPError DatabaseError where
  toErrorCode = \case
    NotPostgresBackend -> "DB_NOT_POSTGRES_BACKEND"
    SQLRequestError _ _ -> "DB_SQL_REQUEST_ERROR"
    SQLResultError _ -> "DB_SQL_RESULT_ERROR"
    DBUnknownError _ -> "DB_UNKNOWN_ERROR"
  toHttpCode _ = E500

instance IsAPIError DatabaseError

data FCMTokenError
  = FCMJSONPathNotConfigured
  | UnableToReadFCMJSONFile
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FCMTokenError

instance IsBaseError FCMTokenError

instance IsHTTPError FCMTokenError where
  toErrorCode FCMJSONPathNotConfigured = "FCM_JSON_PATH_NOT_CONFIGURED"
  toErrorCode UnableToReadFCMJSONFile = "UNABLE_TO_READ_FCM_JSON_FILE"
  toHttpCode _ = E500

instance IsAPIError FCMTokenError

data ContextError
  = UnsupportedCoreVer
  | UnsupportedDomainVer
  | InvalidDomain
  | InvalidCountry
  | InvalidCity
  | InvalidAction
  deriving (Eq, Show)

instanceExceptionWithParent 'HTTPException ''ContextError

instance IsBaseError ContextError

instance IsHTTPError ContextError where
  toErrorCode UnsupportedCoreVer = "UNSUPPORTED_CORE_VERSION"
  toErrorCode UnsupportedDomainVer = "UNSUPPORTED_DOMAIN_VERSION"
  toErrorCode InvalidDomain = "INVALID_DOMAIN"
  toErrorCode InvalidCountry = "INVALID_COUNTRY"
  toErrorCode InvalidCity = "INVALID_CITY"
  toErrorCode InvalidAction = "INVALID_ACTION"
  toHttpCode _ = E400

instance IsAPIError ContextError

instance IsBecknAPIError ContextError where
  toType _ = CONTEXT_ERROR

externalAPICallErrorMessage :: BaseUrl -> ClientError -> Maybe Text
externalAPICallErrorMessage baseUrl clientErr =
  Just $
    "Failure in the external API call to "
      <> showBaseUrlText baseUrl
      <> ": "
      <> show clientErr

data ExternalAPICallError = ExternalAPICallError
  { errCode :: Maybe Text,
    baseUrl :: BaseUrl,
    clientError :: ClientError
  }
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''ExternalAPICallError

instance IsBaseError ExternalAPICallError where
  toMessage (ExternalAPICallError _ url err) = externalAPICallErrorMessage url err

instance IsHTTPError ExternalAPICallError where
  toErrorCode (ExternalAPICallError codeMb _ _) = fromMaybe "EXTERNAL_API_CALL_ERROR" codeMb

instance IsAPIError ExternalAPICallError

data GraphHopperError = GraphHopperError
  { errCode :: Maybe Text,
    baseUrl :: BaseUrl,
    clientError :: ClientError
  }
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''GraphHopperError

instance IsBaseError GraphHopperError where
  toMessage (GraphHopperError _ url err) = externalAPICallErrorMessage url err

instance IsHTTPError GraphHopperError where
  toErrorCode (GraphHopperError codeMb _ _) = fromMaybe "EXTERNAL_API_CALL_ERROR" codeMb
  toHttpCode _ = E400

instance IsAPIError GraphHopperError

newtype EmailSendingError
  = EmailSendingError Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''EmailSendingError

instance IsBaseError EmailSendingError where
  toMessage (EmailSendingError msg) = Just msg

instance IsHTTPError EmailSendingError where
  toErrorCode (EmailSendingError _) = "EMAIL_SENDING_ERROR"

instance IsAPIError EmailSendingError

data HealthCheckError
  = ServiceUnavailable
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''HealthCheckError

instance IsBaseError HealthCheckError

instance IsHTTPError HealthCheckError where
  toErrorCode ServiceUnavailable = "SERVICE_UNAVAILABLE"
  toHttpCode ServiceUnavailable = E503

instance IsAPIError HealthCheckError

data RouteError
  = RouteRequestError BaseUrl ClientError
  | RouteNotLatLong
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RouteError

instance IsBaseError RouteError where
  toMessage = \case
    RouteRequestError url err -> externalAPICallErrorMessage url err
    RouteNotLatLong -> Just "Not supporting waypoints other than LatLong."

instance IsHTTPError RouteError where
  toErrorCode = \case
    RouteRequestError _ _ -> "UNABLE_TO_GET_ROUTE"
    RouteNotLatLong -> "GET_ROUTE_UNSUPPORTED_FORMAT"

instance IsAPIError RouteError

data ServerError
  = ServerUnavailable
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''ServerError

instance IsBaseError ServerError where
  toMessage ServerUnavailable = Just "Server is working, but is not available."

instance IsHTTPError ServerError where
  toErrorCode ServerUnavailable = "SERVER_UNAVAILABLE"
  toHttpCode ServerUnavailable = E503

instance IsAPIError ServerError

newtype RedisError
  = RedisError KVDBReply
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RedisError

instance IsBaseError RedisError where
  toMessage = \case
    RedisError err -> Just $ show err

instance IsHTTPError RedisError where
  toErrorCode = \case
    RedisError _ -> "REDIS_ERROR"
  toHttpCode _ = E500

instance IsAPIError RedisError

newtype ActionNotSupported = ActionNotSupported Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''ActionNotSupported

instance IsBaseError ActionNotSupported where
  toMessage (ActionNotSupported action) = Just $ "Action " <> action <> " is not supported"

instance IsHTTPError ActionNotSupported where
  toErrorCode _ = "ACTION_NOT_SUPPORTED"
  toHttpCode _ = E400

instance IsAPIError ActionNotSupported

data SMSError
  = SMSError SubmitSmsRes
  | SMSInvalidNumber
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SMSError

instance IsBaseError SMSError where
  toMessage = \case
    SMSError err -> Just $ submitSmsResToText err
    _ -> Nothing

instance IsHTTPError SMSError where
  toErrorCode = \case
    SMSError _ -> "SMS_NOT_SENT"
    SMSInvalidNumber -> "SMS_INVALID_NUMBER"

  toHttpCode = \case
    SMSError _ -> E500
    SMSInvalidNumber -> E400

instance IsAPIError SMSError

data GoogleMapsCallError = GoogleMapsInvalidRequest | GoogleMapsCallError Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''GoogleMapsCallError

instance IsBaseError GoogleMapsCallError where
  toMessage = \case
    GoogleMapsInvalidRequest -> Just "Invalid request to Google Maps."
    GoogleMapsCallError googleErrorCode -> Just googleErrorCode

instance IsHTTPError GoogleMapsCallError where
  toErrorCode = \case
    GoogleMapsInvalidRequest -> "GOOGLE_MAPS_INVALID_REQUEST"
    GoogleMapsCallError _ -> "GOOGLE_MAPS_CALL_ERROR"
  toHttpCode = \case
    GoogleMapsInvalidRequest -> E400
    GoogleMapsCallError _ -> E500

instance IsAPIError GoogleMapsCallError

data AgencyDisabled
  = AgencyDisabled
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''AgencyDisabled

instance IsBaseError AgencyDisabled

instance IsHTTPError AgencyDisabled where
  toErrorCode AgencyDisabled = "AGENCY_DISABLED"
  toHttpCode AgencyDisabled = E503

instance IsAPIError AgencyDisabled

data ExotelError
  = ExotelNotConfigured
  | ExotelBadRequest
  | ExotelUnauthorized
  | ExitelPaymentRequired
  | ExotelAccessDenied
  | ExotelNotFound
  | ExotelConflict
  | ExotelTooManyRequests
  | ExotelServerError
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''ExotelError

instance FromResponse ExotelError where
  fromResponse resp = case statusCode $ responseStatusCode resp of
    400 -> Just ExotelBadRequest
    401 -> Just ExotelUnauthorized
    402 -> Just ExitelPaymentRequired
    403 -> Just ExotelAccessDenied
    404 -> Just ExotelNotFound
    409 -> Just ExotelConflict
    429 -> Just ExotelTooManyRequests
    _ -> Just ExotelServerError

instance IsBaseError ExotelError where
  toMessage = \case
    ExotelNotConfigured -> Just "Exotel env variables aren't properly set."
    ExotelBadRequest -> Just "Something in your header or request body was malformed."
    ExotelUnauthorized -> Just "Necessary credentials were either missing or invalid."
    ExitelPaymentRequired -> Just "The action is not available on your plan, or you have exceeded usage limits for your current plan."
    ExotelAccessDenied -> Just "Your credentials are valid, but you don’t have access to the requested resource."
    ExotelNotFound -> Just "The object you’re requesting doesn’t exist."
    ExotelConflict -> Just "You might be trying to update the same resource concurrently."
    ExotelTooManyRequests -> Just "You are calling our APIs more frequently than we allow."
    ExotelServerError -> Just "Something went wrong on our end. Please try again."

instance IsHTTPError ExotelError where
  toErrorCode = \case
    ExotelNotConfigured -> "EXOTEL_NOT_CONFIGURED"
    ExotelBadRequest -> "EXOTEL_BAD_REQUEST"
    ExotelUnauthorized -> "EXOTEL_UNAUTHORIZED"
    ExitelPaymentRequired -> "EXOTEL_PAYMENT_REQUIRED"
    ExotelAccessDenied -> "EXOTEL_ACCESS_DENIED"
    ExotelNotFound -> "EXOTEL_NOT_FOUND"
    ExotelConflict -> "EXOTEL_CONFLICT"
    ExotelTooManyRequests -> "EXOTEL_TOO_MANY_REQUESTS"
    ExotelServerError -> "EXOTEL_SERVER_ERROR"

instance IsAPIError ExotelError

data KafkaError
  = KafkaUnableToBuildTools Kafka.KafkaError
  | KafkaUnableToReleaseTools Kafka.KafkaError
  | KafkaUnableToProduceMessage Kafka.KafkaError
  | KafkaUnableToConsumeMessage Kafka.KafkaError
  | KafkaUnableToParseValue
  | KafkaTopicIsEmptyString
  deriving (Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''KafkaError

instance IsBaseError KafkaError where
  toMessage = \case
    KafkaUnableToBuildTools err -> Just $ "Attemption to build Kafka tools ended with error: " <> show err
    KafkaUnableToReleaseTools err -> Just $ "Attemption to release Kafka tools ended with error: " <> show err
    KafkaUnableToProduceMessage err -> Just $ "Attemption to produce message ended with error: " <> show err
    KafkaUnableToConsumeMessage err -> Just $ "Attemption to consume message ended with error: " <> show err
    KafkaUnableToParseValue -> Just "Unable to parse value of received message."
    KafkaTopicIsEmptyString -> Just "Kafka topic is empty string."

instance IsHTTPError KafkaError where
  toErrorCode = \case
    KafkaUnableToBuildTools _ -> "KAFKA_UNABLE_TO_BUILD_TOOLS"
    KafkaUnableToReleaseTools _ -> "KAFKA_UNABLE_TO_RELEASE_TOOLS"
    KafkaUnableToProduceMessage _ -> "KAFKA_UNABLE_TO_PRODUCE_MESSAGE"
    KafkaUnableToConsumeMessage _ -> "KAFKA_UNABLE_TO_CONSUME_MESSAGE"
    KafkaUnableToParseValue -> "KAFKA_UNABLE_TO_PARSE_VALUE"
    KafkaTopicIsEmptyString -> "KAFKA_TOPIC_IS_EMPTY_STRING"

instance IsAPIError KafkaError

data CallStatusError
  = CallStatusDoesNotExist
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''CallStatusError

instance IsBaseError CallStatusError where
  toMessage CallStatusDoesNotExist = Just "No call callback received yet."

instance IsHTTPError CallStatusError where
  toErrorCode CallStatusDoesNotExist = "CALL_DOES_NOT_EXIST"
  toHttpCode CallStatusDoesNotExist = E400

instance IsAPIError CallStatusError

data ServiceabilityError
  = RideNotServiceable
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''ServiceabilityError

instance IsBaseError ServiceabilityError where
  toMessage RideNotServiceable = Just "Requested ride is not serviceable due to georestrictions."

instance IsHTTPError ServiceabilityError where
  toErrorCode RideNotServiceable = "RIDE_NOT_SERVICEABLE"
  toHttpCode RideNotServiceable = E400

instance IsAPIError ServiceabilityError
