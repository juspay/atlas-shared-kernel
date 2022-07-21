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

Module      :  Beckn.Streaming.Kafka.Topic.BusinessEvent.Functions
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Streaming.Kafka.Topic.BusinessEvent.Functions where

import Beckn.Streaming.Kafka.Topic.BusinessEvent.Environment
import Beckn.Streaming.Kafka.Topic.BusinessEvent.Types
import Beckn.Streaming.MonadProducer (MonadProducer (..))
import Beckn.Types.Logging
import Beckn.Types.Time
import EulerHS.Prelude

produceBusinessEventMessage ::
  ( MonadProducer BusinessEvent m,
    Log m,
    MonadTime m,
    MonadReader r m,
    HasKafkaBE r kafkaEnvs,
    ToJSON a,
    ToJSON b
  ) =>
  KafkaBEName ->
  a ->
  b ->
  m ()
produceBusinessEventMessage eventName metadata payload = do
  event <- buildBusinessEvent eventName metadata payload
  produceMessage (Just $ encodeUtf8 event.eventName) event
