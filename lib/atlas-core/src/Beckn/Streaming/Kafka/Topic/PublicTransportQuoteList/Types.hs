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

Module      :  Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList.Types
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList.Types where

import Beckn.Prelude
import qualified Beckn.Streaming.Kafka.Consumer as Cons
import Beckn.Streaming.Kafka.Consumer.Types (HasKafkaConsumer, KafkaConsumerTools)
import Beckn.Streaming.Kafka.HasKafkaTopics (HasKafkaTopics (..))
import qualified Beckn.Streaming.Kafka.Producer as Prod
import Beckn.Streaming.Kafka.Producer.Types
import Beckn.Streaming.MonadConsumer (MonadConsumer (..))
import Beckn.Streaming.MonadProducer (MonadProducer (..))
import Beckn.Types.Amount
import Beckn.Types.Flow (FlowR)
import Beckn.Types.Logging

type HasKafkaPublicTransportQuotesConsumer env r =
  ( HasKafkaConsumer env r,
    HasField "publicTransportQuotes" env (KafkaConsumerTools PublicTransportQuoteList)
  )

type TransactionId = Text

data PublicTransportStation = PublicTransportStation
  { name :: Text,
    stationCode :: Text,
    lat :: Double,
    lon :: Double
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data PublicTransportQuote = PublicTransportQuote
  { id :: Text,
    description :: Text,
    fare :: Amount,
    departureTime :: UTCTime,
    arrivalTime :: UTCTime,
    departureStation :: PublicTransportStation,
    arrivalStation :: PublicTransportStation,
    createdAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data PublicTransportQuoteList = PublicTransportQuoteList
  { transactionId :: TransactionId,
    quoteList :: [PublicTransportQuote]
  }
  deriving (Generic, FromJSON, ToJSON)

instance HasKafkaTopics PublicTransportQuoteList where
  getTopics = ["public_transport_bap_quote_list"]

instance (Log (FlowR r), HasKafkaProducer r) => MonadProducer PublicTransportQuoteList (FlowR r) where
  type Args PublicTransportQuoteList = ()
  produceMessage () value = mapM_ ($ value) (Prod.produceMessage <$> map (,Nothing) (getTopics @PublicTransportQuoteList))

instance (Log (FlowR r), HasKafkaPublicTransportQuotesConsumer env r) => MonadConsumer PublicTransportQuoteList (FlowR r) where
  receiveMessage = asks (.kafkaConsumerEnv.publicTransportQuotes) >>= Cons.receiveMessage
