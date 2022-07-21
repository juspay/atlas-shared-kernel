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

Module      :  Beckn.Streaming.Kafka.Topic.PublicTransportSearch.Types
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Streaming.Kafka.Topic.PublicTransportSearch.Types where

import Beckn.Prelude
import qualified Beckn.Streaming.Kafka.Consumer as Cons
import Beckn.Streaming.Kafka.Consumer.Types (HasKafkaConsumer, KafkaConsumerTools)
import Beckn.Streaming.Kafka.HasKafkaTopics (HasKafkaTopics (..))
import qualified Beckn.Streaming.Kafka.Producer as Prod
import Beckn.Streaming.Kafka.Producer.Types
import Beckn.Streaming.MonadConsumer (MonadConsumer (..))
import Beckn.Streaming.MonadProducer (MonadProducer (..))
import Beckn.Types.Flow (FlowR)
import Beckn.Types.Logging
import Beckn.Types.MapSearch (LatLong)

type HasKafkaPublicTransportSearchConsumer env r =
  ( HasKafkaConsumer env r,
    HasField "publicTransportSearch" env (KafkaConsumerTools PublicTransportSearch)
  )

type SearchId = Text

type PersonId = Text

data PublicTransportSearch = PublicTransportSearch
  { id :: SearchId,
    gps :: LatLong,
    requestorId :: PersonId,
    createdAt :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON)

instance HasKafkaTopics PublicTransportSearch where
  getTopics = ["public_transport_bap_search"]

instance (Log (FlowR r), HasKafkaProducer r) => MonadProducer PublicTransportSearch (FlowR r) where
  type Args PublicTransportSearch = ()
  produceMessage () value = mapM_ ($ value) (Prod.produceMessage <$> map (,Nothing) (getTopics @PublicTransportSearch))

instance (Log (FlowR r), HasKafkaPublicTransportSearchConsumer env r) => MonadConsumer PublicTransportSearch (FlowR r) where
  receiveMessage = asks (.kafkaConsumerEnv.publicTransportSearch) >>= Cons.receiveMessage
