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

Module      :  Beckn.Utils.FlowLogging

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.FlowLogging
  ( LoggerConfig (..),
    getEulerLoggerRuntime,
    appendLogContext,
    logOutputImplementation,
    withLogTagImplementation,
  )
where

import Beckn.Types.Logging
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Txt
import qualified Data.Time as Time
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Runtime
import EulerHS.Types (LogContext)
import qualified EulerHS.Types as T
import qualified Prelude as P

logOutputImplementation :: L.MonadFlow m => LogLevel -> T.Message -> m ()
logOutputImplementation logLevel message =
  case logLevel of
    DEBUG -> L.logDebug EmtpyTag message
    INFO -> L.logInfo EmtpyTag message
    WARNING -> L.logWarning EmtpyTag message
    ERROR -> L.logError EmtpyTag message

withLogTagImplementation ::
  L.MonadFlow m =>
  Text ->
  ReaderT r L.Flow a ->
  ReaderT r m a
withLogTagImplementation lc flowR =
  ReaderT $
    L.withLoggerContext (appendLogContext lc)
      . runReaderT flowR

data EmtpyTag = EmtpyTag

instance P.Show EmtpyTag where
  show _ = ""

formatTag :: Text -> Text
formatTag tag = "[" <> tag <> "]"

appendLogContext :: Text -> LogContext -> LogContext
appendLogContext val lc =
  let oldLCText = fromMaybe "" $ HM.lookup logContextKey lc
   in HM.insert logContextKey (oldLCText <> formatTag val) lc

getEulerLoggerConfig :: LoggerConfig -> T.LoggerConfig
getEulerLoggerConfig LoggerConfig {..} =
  T.defaultLoggerConfig
    { T._isAsync = True,
      T._logLevel = logLevel,
      T._logToFile = logToFile,
      T._logFilePath = eulLogsFilePath,
      T._logToConsole = logToConsole,
      T._logRawSql = logSql
    }
  where
    logLevel = case level of
      DEBUG -> T.Debug
      INFO -> T.Info
      WARNING -> T.Warning
      ERROR -> T.Error
    logSql =
      if logRawSql
        then T.UnsafeLogSQL_DO_NOT_USE_IN_PRODUCTION
        else T.SafelyOmitSqlLogs
    eulLogsFilePath = do
      let (l, r) = Text.breakOnEnd "." (Text.pack logFilePath)
      Text.unpack $
        if null l
          then r <> "-eul"
          else Text.init l <> "-eul." <> r

getEulerLoggerRuntime :: Maybe Text -> LoggerConfig -> IO LoggerRuntime
getEulerLoggerRuntime hostname = createLoggerRuntime (logFlowFormatter hostname) . getEulerLoggerConfig

logFlowFormatter :: Maybe Text -> T.FlowFormatter
logFlowFormatter hostname _ = do
  currTime <- Time.getCurrentTime
  pure $ logFormatterText currTime hostname

logFormatterText :: Time.UTCTime -> Maybe Text -> T.MessageFormatter
logFormatterText
  timestamp
  hostname
  (T.PendingMsg _mbFlowGuid elvl eTag msg msgNum logContHM) = res
    where
      logCont = HM.lookupDefault "" logContextKey logContHM
      tag = if null eTag || eTag == "\"\"" then "" else formatTag eTag
      lvl = case elvl of
        T.Debug -> DEBUG
        T.Warning -> WARNING
        T.Info -> INFO
        T.Error -> ERROR
      textToLBS = LBS.fromStrict . Txt.encodeUtf8
      res =
        T.SimpleLBS $
          show timestamp
            <> " "
            <> show lvl
            <> " "
            <> show msgNum
            <> "> @"
            <> textToLBS (fromMaybe "null" hostname)
            <> " "
            <> textToLBS logCont
            <> textToLBS tag
            <> " |> "
            <> textToLBS msg

logContextKey :: Text
logContextKey = "log_context"
