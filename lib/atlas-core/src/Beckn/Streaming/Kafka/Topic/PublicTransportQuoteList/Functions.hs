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

Module      :  Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList.Functions
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList.Functions where

import Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList.Types
import Beckn.Streaming.MonadProducer (MonadProducer (..))
import EulerHS.Prelude

producePublicTransportQuoteListMessage ::
  ( MonadProducer PublicTransportQuoteList m
  ) =>
  TransactionId ->
  [PublicTransportQuote] ->
  m ()
producePublicTransportQuoteListMessage txnId = do
  produceMessage () . PublicTransportQuoteList txnId