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

Module      :  Beckn.Types.Core.Migration.Fulfillment
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Migration.Fulfillment where

import Beckn.Types.Core.Migration.Agent (Agent)
import Beckn.Types.Core.Migration.Contact (Contact)
import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Location (Location)
import Beckn.Types.Core.Migration.Person (Person)
import Beckn.Types.Core.Migration.State (State)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Types.Core.Migration.Vehicle (Vehicle)
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Aeson (withObject, (.!=), (.:?))
import EulerHS.Prelude hiding (State, id, state)

data Fulfillment = Fulfillment
  { id :: Maybe Text,
    _type :: Maybe Text,
    provider_id :: Maybe Text,
    state :: Maybe State,
    tracking :: Bool,
    customer :: Maybe FulfillmentParticipant,
    agent :: Maybe Agent,
    vehicle :: Maybe Vehicle,
    start :: Maybe FulfillmentDetails,
    end :: Maybe FulfillmentDetails,
    tags :: Maybe Tags
  }
  deriving (Generic, Show)

emptyFulfillment :: Fulfillment
emptyFulfillment =
  Fulfillment
    { id = Nothing,
      _type = Nothing,
      provider_id = Nothing,
      state = Nothing,
      tracking = False,
      customer = Nothing,
      agent = Nothing,
      vehicle = Nothing,
      start = Nothing,
      end = Nothing,
      tags = Nothing
    }

instance FromJSON Fulfillment where
  parseJSON = withObject "Fulfillment" $ \o ->
    Fulfillment
      <$> o .:? "id"
      <*> o .:? "type"
      <*> o .:? "provider_id"
      <*> o .:? "state"
      <*> o .:? "tracking" .!= False
      <*> o .:? "customer"
      <*> o .:? "agent"
      <*> o .:? "vehicle"
      <*> o .:? "start"
      <*> o .:? "end"
      <*> o .:? "tags"

instance ToJSON Fulfillment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Fulfillment where
  example =
    Fulfillment
      { id = Nothing,
        _type = Nothing,
        provider_id = Nothing,
        state = Nothing,
        tracking = False,
        customer = Nothing,
        agent = Nothing,
        vehicle = Nothing,
        start = Nothing,
        end = Nothing,
        tags = Nothing
      }

data FulfillmentParticipant = FulfillmentParticipant
  { person :: Maybe Person,
    contact :: Maybe Contact
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data FulfillmentDetails = FulfillmentDetails
  { location :: Maybe Location,
    time :: Maybe Time,
    instructions :: Maybe Descriptor,
    contact :: Maybe Contact,
    person :: Maybe Person
  }
  deriving (Generic, FromJSON, ToJSON, Show)

emptyFulfillmentDetails :: FulfillmentDetails
emptyFulfillmentDetails =
  FulfillmentDetails
    { location = Nothing,
      time = Nothing,
      instructions = Nothing,
      contact = Nothing,
      person = Nothing
    }
