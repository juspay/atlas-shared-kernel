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

Module      :  Beckn.Streaming.Kafka.Producer.Types

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Streaming.Kafka.Producer.Types
  ( KafkaProducerCfg (..),
    KafkaProducerTools,
    buildKafkaProducerTools,
    releaseKafkaProducerTools,
    HasKafkaProducer,
    module Reexport,
  )
where

import Beckn.Streaming.Kafka.Commons as Reexport
import Beckn.Types.Error
import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude
import GHC.Records.Extra (HasField)
import Kafka.Producer as Producer

type HasKafkaProducer r = HasField "kafkaProducerTools" r KafkaProducerTools

newtype KafkaProducerCfg = KafkaProducerCfg
  { brokers :: KafkaBrokersList
  }
  deriving (Generic, FromDhall)

newtype KafkaProducerTools = KafkaProducerTools
  { producer :: Producer.KafkaProducer
  }
  deriving (Generic)

producerProps :: KafkaProducerCfg -> ProducerProperties
producerProps kafkaProducerCfg =
  brokersList castBrokers
    <> logLevel KafkaLogDebug
  where
    castBrokers = BrokerAddress <$> kafkaProducerCfg.brokers

buildKafkaProducerTools :: KafkaProducerCfg -> IO KafkaProducerTools
buildKafkaProducerTools kafkaProducerCfg = do
  producer <- newProducer (producerProps kafkaProducerCfg) >>= either (throwM . KafkaUnableToBuildTools) return
  return $ KafkaProducerTools {..}

releaseKafkaProducerTools :: KafkaProducerTools -> IO ()
releaseKafkaProducerTools kafkaProducerTools = closeProducer kafkaProducerTools.producer
