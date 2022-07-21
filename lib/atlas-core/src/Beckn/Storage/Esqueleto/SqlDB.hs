{-# LANGUAGE DerivingStrategies #-}


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

Module      :  Beckn.Storage.Esqueleto.SqlDB

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Storage.Esqueleto.SqlDB
  ( SqlDBEnv (..),
    SqlDB,
  )
where

import Beckn.Storage.Esqueleto.Logger (LoggerIO)
import Beckn.Types.GuidLike
import Beckn.Types.MonadGuid
import Beckn.Types.Time (MonadTime (..))
import Beckn.Utils.Logging
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental (SqlBackend)
import EulerHS.Prelude

newtype SqlDBEnv = SqlDBEnv
  { currentTime :: UTCTime
  }

type SqlDB a = ReaderT SqlDBEnv (ReaderT SqlBackend LoggerIO) a

instance Monad m => MonadTime (ReaderT SqlDBEnv m) where
  getCurrentTime = asks (.currentTime)

instance MonadGuid (ReaderT SqlDBEnv (ReaderT SqlBackend LoggerIO)) where
  generateGUIDText = lift $ lift generateGUID

instance Log (ReaderT SqlDBEnv (ReaderT SqlBackend LoggerIO)) where
  logOutput a b = lift . lift $ logOutput a b
  withLogTag a (ReaderT f1) = ReaderT $ \env1 -> do
    let (ReaderT f2) = f1 env1
    ReaderT $ \env2 ->
      withLogTag a $ f2 env2
