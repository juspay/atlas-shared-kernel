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

Module      :  Beckn.Storage.Redis.Config
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Storage.Redis.Config (prepareRedisConnections, AppException (..), T.RedisConfig) where

import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as T

prepareRedisConnections :: (L.MonadFlow mFlow, Log mFlow) => T.RedisConfig -> mFlow ()
prepareRedisConnections redisCfg = do
  L.getOrInitKVDBConn (T.mkKVDBConfig "redis" redisCfg) >>= throwOnFailedWithLog
  L.runKVDB "redis" (L.setex "dummy" 1 "dummy") >>= throwOnFailedWithLog
  where
    throwOnFailedWithLog (Left err) = do
      logTagError "" $ errmsg err
      L.throwException $ KVDBConnectionFailedException $ errmsg err
    throwOnFailedWithLog _ = pure ()
    errmsg err = "Failed to get or initialize connection to Redis. " <> show err

newtype AppException
  = KVDBConnectionFailedException Text
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Exception)
