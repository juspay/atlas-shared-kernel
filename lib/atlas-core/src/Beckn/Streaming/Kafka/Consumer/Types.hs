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

Module      :  Beckn.Streaming.Kafka.Consumer.Types

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Streaming.Kafka.Consumer.Types
  ( KafkaConsumerCfg (..),
    KafkaConsumerTools,
    HasKafkaConsumer,
    buildKafkaConsumerTools,
    releaseKafkaConsumerTools,
    module Reexport,
  )
where

import Beckn.Streaming.Kafka.Commons as Reexport
import Beckn.Streaming.Kafka.HasKafkaTopics
import Beckn.Types.Error
import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude
import GHC.Records.Extra (HasField)
import Kafka.Consumer hiding (ConsumerGroupId, groupId)
import qualified Kafka.Consumer as Consumer

type HasKafkaConsumer env r = HasField "kafkaConsumerEnv" r env

type ConsumerGroupId = Text

data KafkaConsumerCfg = KafkaConsumerCfg
  { brokers :: KafkaBrokersList,
    groupId :: ConsumerGroupId,
    timeoutMilliseconds :: Int
  }
  deriving (Generic, FromDhall)

data KafkaConsumerTools a = KafkaConsumerTools
  { kafkaConsumerCfg :: KafkaConsumerCfg,
    consumer :: Consumer.KafkaConsumer
  }
  deriving (Generic)

consumerProps :: KafkaConsumerCfg -> ConsumerProperties
consumerProps kafkaConsumerCfg =
  brokersList castBrokers
    <> Consumer.groupId (Consumer.ConsumerGroupId kafkaConsumerCfg.groupId)
    <> logLevel KafkaLogDebug
  where
    castBrokers = BrokerAddress <$> kafkaConsumerCfg.brokers

consumerSub :: [KafkaTopic] -> Subscription
consumerSub topicList =
  Consumer.topics castTopics
    <> offsetReset Earliest
  where
    castTopics = TopicName <$> topicList

buildKafkaConsumerTools :: forall a. HasKafkaTopics a => KafkaConsumerCfg -> IO (KafkaConsumerTools a)
buildKafkaConsumerTools kafkaConsumerCfg = do
  consumer <-
    newConsumer (consumerProps kafkaConsumerCfg) (consumerSub $ getTopics @a)
      >>= either (throwM . KafkaUnableToBuildTools) return

  return $ KafkaConsumerTools {..}

releaseKafkaConsumerTools :: KafkaConsumerTools a -> IO ()
releaseKafkaConsumerTools kafkaConsumerTools =
  closeConsumer kafkaConsumerTools.consumer
    >>= flip whenJust (throwM . KafkaUnableToReleaseTools)
