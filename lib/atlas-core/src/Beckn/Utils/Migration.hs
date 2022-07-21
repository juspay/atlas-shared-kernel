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

Module      :  Beckn.Utils.Migration

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Migration
  ( migrateIfNeeded,
  )
where

import Beckn.Prelude
import Beckn.Storage.DB.Config
import Beckn.Storage.Esqueleto.Migration (migrateIfNeeded')
import Beckn.Types.Common
import Control.Exception.Safe
import qualified Database.PostgreSQL.Simple as PS
import qualified EulerHS.Types as T

fromPgConfig :: T.PostgresConfig -> PS.ConnectInfo
fromPgConfig T.PostgresConfig {..} = PS.ConnectInfo {..}

migrateIfNeeded :: (MonadMask m, MonadIO m, Log m) => Maybe FilePath -> Bool -> DBConfig -> m (Either String ())
migrateIfNeeded mPath autoMigrate dbConfig =
  migrateIfNeeded' mPath autoMigrate schemaName connectInfo
  where
    schemaName = encodeUtf8 dbConfig.schemaName
    connectInfo = fromPgConfig dbConfig.pgConfig
