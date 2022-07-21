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

Module      :  Beckn.Streaming.Kafka.Consumer

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Streaming.Kafka.Consumer
  ( Beckn.Streaming.Kafka.Consumer.receiveMessage,
    listenForMessages,
  )
where

import Beckn.Prelude
import Beckn.Streaming.Kafka.Consumer.Types as ConsTypes
import qualified Beckn.Streaming.MonadConsumer as MonadCons
import Beckn.Types.Error
import Beckn.Utils.Error.Throwing (fromMaybeM, throwError)
import Beckn.Utils.Logging
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Kafka.Consumer as KafkaCons

receiveMessage :: (MonadIO m, Log m, MonadThrow m, FromJSON a) => KafkaConsumerTools a -> m (Maybe a)
receiveMessage kafkaConsumerTools = withLogTag "KafkaConsumer" $ do
  let timeout = kafkaConsumerTools.kafkaConsumerCfg.timeoutMilliseconds
  etrMsg <- pollMessage kafkaConsumerTools.consumer (Timeout timeout)
  case etrMsg of
    Left err -> handleResponseError err
    Right res -> do
      mbErr <- commitAllOffsets OffsetCommit kafkaConsumerTools.consumer
      whenJust mbErr $ \err -> logError $ "Unable to commit offsets: " <> show err
      crValue res >>= A.decode . LBS.fromStrict
        & fromMaybeM KafkaUnableToParseValue
        <&> Just
  where
    handleResponseError err =
      case err of
        KafkaResponseError RdKafkaRespErrTimedOut -> do
          logInfo "No messages to consume."
          return Nothing
        _ -> throwError $ KafkaUnableToConsumeMessage err

listenForMessages ::
  ( MonadCons.MonadConsumer a m,
    MonadIO m,
    MonadCatch m,
    Log m,
    MonadThrow m
  ) =>
  m Bool ->
  (a -> m ()) ->
  m ()
listenForMessages isRunning handle = whileM isRunning $ do
  etrRes <- try @_ @SomeException MonadCons.receiveMessage
  case etrRes of
    Left err -> logInfo $ "Message was not received: " <> show err
    Right res -> forM_ res handle
