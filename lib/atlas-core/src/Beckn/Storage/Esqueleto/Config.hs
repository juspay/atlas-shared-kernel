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

Module      :  Beckn.Storage.Esqueleto.Config
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Storage.Esqueleto.Config where

import Beckn.Storage.Esqueleto.Logger (runLoggerIO)
import Beckn.Types.App (HasFlowEnv)
import Beckn.Types.Field ((:::))
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Data.Pool (Pool)
import Database.Persist.Postgresql
import Database.PostgreSQL.Simple (execute_)
import Database.PostgreSQL.Simple.Types (Query (Query))
import EulerHS.Prelude

data EsqDBConfig = EsqDBConfig
  { connectHost :: Text,
    connectPort :: Word16,
    connectUser :: Text,
    connectPassword :: Text,
    connectDatabase :: Text,
    connectSchemaName :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, FromDhall)

newtype EsqDBEnv = EsqDBEnv
  { connPool :: Pool SqlBackend
  }
  deriving (Generic)

prepareEsqDBEnv :: EsqDBConfig -> LoggerEnv -> IO EsqDBEnv
prepareEsqDBEnv cfg logEnv = do
  let connStr = makeConnString cfg
      modifyConnString = encodeUtf8 cfg.connectSchemaName
  let checkedLogEnv =
        if logEnv.logRawSql
          then logEnv
          else logEnv {fileLogger = Nothing, consoleLogger = Nothing}
  pool <- liftIO . runLoggerIO checkedLogEnv $ createPostgresqlPoolModified (modifyConn modifyConnString) connStr 10
  return $ EsqDBEnv pool
  where
    makeConnString dbConfig =
      encodeUtf8 $
        "host=" <> dbConfig.connectHost
          <> " dbname="
          <> dbConfig.connectDatabase
          <> " user="
          <> dbConfig.connectUser
          <> " password="
          <> dbConfig.connectPassword
          <> " port="
          <> show dbConfig.connectPort
    modifyConn schemaName conn =
      void . execute_ conn . Query $ "set search_path to " <> schemaName <> ", public; "

type EsqDBFlow m r = (HasFlowEnv m r '["esqDBEnv" ::: EsqDBEnv], HasLog r)
