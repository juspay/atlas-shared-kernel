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

Module      :  Beckn.Types.Core.Metro.Search.Payment

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Metro.Search.Payment
  ( Payment (..),
    PaymentType (..),
    TLMethod (..),
    Params (..),
  )
where

import Beckn.Types.Core.Migration.DecimalValue (DecimalValue)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Aeson (Value (..), object, withObject, (.:), (.=))
import Data.Aeson.Types (typeMismatch)
import Data.HashMap.Strict (delete)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (State, (.=))
import Servant.Client (BaseUrl)

data Payment = Payment
  { uri :: Maybe BaseUrl,
    tl_method :: Maybe TLMethod,
    params :: Maybe Params,
    _type :: Maybe PaymentType,
    status :: Maybe Status,
    time :: Maybe Time
  }
  deriving (Generic, Show, ToSchema)

data TLMethod = HttpGet | HttpPost
  deriving (Generic, Show, ToSchema)

instance FromJSON TLMethod where
  parseJSON (String "http/get") = pure HttpGet
  parseJSON (String "http/post") = pure HttpPost
  parseJSON e = typeMismatch "tl_method string" e

instance ToJSON TLMethod where
  toJSON HttpGet = String "http/get"
  toJSON HttpPost = String "http/post"

data Params = Params
  { transaction_id :: Maybe Text,
    transaction_status :: Maybe Text,
    amount :: Maybe DecimalValue,
    currency :: Text,
    additional :: HashMap Text Text
  }
  deriving (Generic, Eq, Show, ToSchema)

instance FromJSON Params where
  parseJSON = withObject "Params" $ \o ->
    Params
      <$> o .: "transaction_id"
      <*> o .: "transaction_status"
      <*> o .: "amount"
      <*> o .: "currency"
      <*> mapM f (additional o)
    where
      f (String val) = pure val
      f e = typeMismatch "additional property of Params" e
      additional =
        delete "transaction_id"
          . delete "transaction_status"
          . delete "amount"
          . delete "currency"

instance ToJSON Params where
  toJSON Params {..} = uniteObjects [object knownParams, Object (String <$> additional)]
    where
      knownParams =
        [ "transaction_id" .= transaction_id,
          "transaction_status" .= transaction_status,
          "amount" .= amount,
          "currency" .= currency
        ]

data BankAccount = BankAccount
  { ifsc_code :: Maybe Text,
    account_number :: Maybe Text,
    account_holder_name :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Eq, Show)

data PaymentType
  = ON_ORDER
  | PRE_FULFILLMENT
  | ON_FULFILLMENT
  | POST_FULFILLMENT
  deriving (Generic, Eq, Show, ToSchema)

data Status = PAID | NOT_PAID
  deriving (Generic, Eq, Show, ToSchema)

instance FromJSON Payment where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Payment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Payment where
  example =
    Payment
      { uri = Nothing,
        tl_method = Nothing,
        params = Nothing,
        _type = Nothing,
        status = Nothing,
        time = Nothing
      }

instance FromJSON PaymentType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentType where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON Status where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON Status where
  toJSON = genericToJSON constructorsWithHyphens
