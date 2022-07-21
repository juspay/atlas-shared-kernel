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

Module      :  Beckn.Types.Logging
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Logging where

import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude

data LogLevel = DEBUG | INFO | WARNING | ERROR
  deriving (Generic, Show, Eq, Ord, FromDhall, ToJSON)

class Log m where
  logOutput :: LogLevel -> Text -> m ()
  withLogTag :: Text -> m a -> m a

data LoggerConfig = LoggerConfig
  { level :: LogLevel,
    logToFile :: Bool,
    logFilePath :: FilePath,
    logToConsole :: Bool,
    logRawSql :: Bool,
    prettyPrinting :: Bool
  }
  deriving (Generic, FromDhall)

defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig =
  LoggerConfig
    { level = DEBUG,
      logFilePath = "/tmp/default",
      logToFile = False,
      logToConsole = False,
      logRawSql = False,
      prettyPrinting = False
    }
