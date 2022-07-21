{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}


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

Module      :  Beckn.Storage.DB.Config
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Storage.DB.Config where

import Beckn.Types.App (MonadFlow)
import Beckn.Types.Flow
import Beckn.Types.Schema
import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude
import qualified EulerHS.Types as T
import GHC.Records.Extra (HasField (..))

data DBConfig = DBConfig
  { connTag :: T.ConnTag,
    pgConfig :: T.PostgresConfig,
    poolConfig :: T.PoolConfig,
    schemaName :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, FromDhall)

-- Make the compiler generate instances for us!
type HasDbCfg r = (HasField "dbCfg" r DBConfig)

type DBFlow m r =
  ( MonadReader r m,
    HasField "dbCfg" r DBConfig,
    MonadFlow m,
    HasSchemaName m
  )

instance HasDbCfg r => HasSchemaName (FlowR r) where
  getSchemaName =
    asks (.dbCfg.schemaName)
