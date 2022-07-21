{-# LANGUAGE AllowAmbiguousTypes #-}


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

Module      :  Beckn.Utils.CacheRedis

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.CacheRedis
  ( getKey,
    setKey,
    delKey,
    setKeyEx,
  )
where

import Beckn.Prelude
import Beckn.Storage.Redis.Queries
import Beckn.Types.Common

type Key = Text

getKey ::
  ( HasCallStack,
    MonadFlow m,
    FromJSON a
  ) =>
  Text ->
  Key ->
  m (Maybe a)
getKey prefix key = getKeyRedis (mkKey prefix key)

setKey ::
  ( HasCallStack,
    MonadFlow m,
    ToJSON a
  ) =>
  Text ->
  Key ->
  a ->
  m ()
setKey prefix key = setKeyRedis (mkKey prefix key)

setKeyEx ::
  ( HasCallStack,
    MonadFlow m,
    ToJSON a
  ) =>
  Text ->
  Seconds ->
  Key ->
  a ->
  m ()
setKeyEx prefix ttl key val = setExRedis (mkKey prefix key) val ttl.getSeconds

delKey ::
  (HasCallStack, MonadFlow m) =>
  Text ->
  Key ->
  m ()
delKey prefix key = void $ deleteKeyRedis (mkKey prefix key)

mkKey :: Text -> Key -> Key
mkKey prefix key = prefix <> ":" <> key
