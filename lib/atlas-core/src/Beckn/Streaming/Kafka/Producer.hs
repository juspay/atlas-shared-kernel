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

Module      :  Beckn.Streaming.Kafka.Producer

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Streaming.Kafka.Producer
  ( buildKafkaProducerTools,
    Beckn.Streaming.Kafka.Producer.produceMessage,
    releaseKafkaProducerTools,
    (..=),
    A.Value (Object),
    A.emptyObject,
  )
where

import Beckn.Streaming.Kafka.Producer.Types
import Beckn.Types.Error
import Beckn.Types.Logging
import Beckn.Utils.Error.Throwing (throwError)
import Data.Aeson (encode)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Lazy as HM
import EulerHS.Prelude
import Kafka.Producer as KafkaProd

produceMessage :: (Log m, MonadThrow m, MonadIO m, MonadReader r m, HasKafkaProducer r, ToJSON a) => (KafkaTopic, Maybe KafkaKey) -> a -> m ()
produceMessage (topic, key) event = do
  kafkaProducerTools <- asks (.kafkaProducerTools)
  when (null topic) $ throwM KafkaTopicIsEmptyString
  mbErr <- KafkaProd.produceMessage kafkaProducerTools.producer message
  whenJust mbErr (throwError . KafkaUnableToProduceMessage)
  where
    message =
      ProducerRecord
        { prTopic = TopicName topic,
          prPartition = UnassignedPartition,
          prKey = key,
          prValue = Just . LBS.toStrict $ encode event
        }

(..=) :: ToJSON a => Text -> a -> HM.HashMap Text A.Value
(..=) = (A..=)
