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

Module      :  Beckn.Utils.CacheMVarEx
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.CacheMVarEx where

import Beckn.Prelude
import Beckn.Types.Cache (CacheKey)
import Beckn.Types.Time
import Beckn.Utils.MVar
import Beckn.Utils.Time
import qualified Data.Map as Map

data CacheMVarEx a = CacheMVarEx
  { cache :: MVar (Map.Map (CacheKey a) a),
    rmQueue :: MVar (Map.Map UTCTime [CacheKey a])
  }

cacheRemover ::
  ( MonadReader r m,
    MonadTime m,
    MonadIO m,
    Ord (CacheKey a)
  ) =>
  (r -> CacheMVarEx a) ->
  m b
cacheRemover cacheAccessor = forever do
  CacheMVarEx {..} <- asks cacheAccessor
  now <- getCurrentTime
  toRemove <-
    join <$> modifyMVar' rmQueue \queue ->
      let (map snd . Map.toList -> less, eq, greater) = Map.splitLookup now queue
       in (greater, maybe less (: less) eq)
  modifyMVar_' cache (`Map.difference` Map.fromList (map (,()) toRemove))
  liftIO . threadDelay . getMicroseconds $ secondsToMcs cacheDelay

cacheDelay :: Seconds
cacheDelay = 5

addToRmQueue ::
  ( MonadTime m,
    MonadReader r m,
    MonadIO m
  ) =>
  (r -> CacheMVarEx a) ->
  Seconds ->
  CacheKey a ->
  m ()
addToRmQueue cacheAccessor ttl key = do
  expTime <- getCurrentTime <&> addUTCTime (fromIntegral ttl)
  CacheMVarEx {rmQueue} <- asks cacheAccessor
  modifyMVar_' rmQueue $ Map.adjust (key :) expTime

setKey ::
  ( MonadReader r m,
    MonadIO m,
    Ord (CacheKey a)
  ) =>
  (r -> CacheMVarEx a) ->
  CacheKey a ->
  a ->
  m ()
setKey cache key val =
  asks cache
    >>= flip modifyMVar_' (Map.insert key val) . (.cache)

setKeyEx ::
  ( MonadReader r m,
    MonadIO m,
    MonadTime m,
    Ord (CacheKey a)
  ) =>
  (r -> CacheMVarEx a) ->
  Seconds ->
  CacheKey a ->
  a ->
  m ()
setKeyEx cacheAccessor ttl key val = do
  setKey cacheAccessor key val
  addToRmQueue cacheAccessor ttl key
