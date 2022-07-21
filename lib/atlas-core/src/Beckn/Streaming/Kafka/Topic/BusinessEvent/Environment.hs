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

Module      :  Beckn.Streaming.Kafka.Topic.BusinessEvent.Environment
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Streaming.Kafka.Topic.BusinessEvent.Environment where

import Beckn.Streaming.Kafka.Producer.Types
import Beckn.Utils.App (getPodName)
import EulerHS.Prelude
import GHC.Records.Extra (HasField)

type HasKafkaBE r kafkaEnvs = (HasField "kafkaEnvs" r kafkaEnvs, HasField "businessEventEnv" kafkaEnvs KafkaBEEnv)

data KafkaBEEnv = KafkaBEEnv
  { hostName :: KafkaHostName,
    serviceName :: KafkaServiceName
  }
  deriving (Generic)

buildKafkaBEEnv :: KafkaServiceName -> IO KafkaBEEnv
buildKafkaBEEnv serviceName = do
  hostName <- getPodName
  return $
    KafkaBEEnv
      { ..
      }
