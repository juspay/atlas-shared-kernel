{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


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

Module      :  Beckn.Storage.Esqueleto.Logger
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Storage.Esqueleto.Logger (LoggerIO (..), runLoggerIO) where

import Beckn.Types.Logging as BLogging (Log (..), LogLevel (..))
import Beckn.Types.MonadGuid
import Beckn.Types.Time (MonadTime (..))
import Beckn.Utils.IOLogging (LoggerEnv, appendLogTag, logOutputIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger as CMLogger
  ( LogLevel (..),
    MonadLogger (..),
    MonadLoggerIO (..),
    ToLogStr (toLogStr),
    fromLogStr,
  )
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import EulerHS.Prelude hiding (Key)

--TODO: Remove this when we remove EulerHS
newtype LoggerIO a = LoggerIO (ReaderT LoggerEnv IO a)
  deriving stock (Generic)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow, MonadCatch, MonadMask)

instance MonadTime LoggerIO where
  getCurrentTime = liftIO getCurrentTime

runLoggerIO :: LoggerEnv -> LoggerIO a -> IO a
runLoggerIO logEnv (LoggerIO rdr) = runReaderT rdr logEnv

logFunc :: ToLogStr msg => LoggerEnv -> BLogging.LogLevel -> msg -> IO ()
logFunc logEnv logLevel msg =
  logOutputIO logEnv logLevel . decodeUtf8 . fromLogStr $ toLogStr msg

logLevelCMtoB :: CMLogger.LogLevel -> BLogging.LogLevel
logLevelCMtoB cmLogLevel = case cmLogLevel of
  LevelError -> ERROR
  LevelWarn -> WARNING
  LevelDebug -> DEBUG
  _ -> INFO

instance MonadLogger LoggerIO where
  monadLoggerLog _ _ logLevel msg = LoggerIO $ do
    loggerEnv <- ask
    liftIO $ logFunc loggerEnv (logLevelCMtoB logLevel) msg

instance MonadLoggerIO LoggerIO where
  askLoggerIO =
    LoggerIO $
      (\logEnv _ _ logLvl msg -> logFunc logEnv (logLevelCMtoB logLvl) msg) <$> ask

instance Log LoggerIO where
  logOutput logLevel msg = LoggerIO $ do
    loggerEnv <- ask
    liftIO $ logFunc loggerEnv logLevel msg

  withLogTag tag (LoggerIO logger) = LoggerIO $ local modifyEnv logger
    where
      modifyEnv logEnv = appendLogTag tag logEnv

instance MonadGuid LoggerIO where
  generateGUIDText = liftIO (UUID.toText <$> UUID.nextRandom)
