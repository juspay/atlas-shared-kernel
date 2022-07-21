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

Module      :  Beckn.Storage.Redis.Queries

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Storage.Redis.Queries
  ( setKeyRedis,
    setExRedis,
    getKeyRedis,
    getKeyRedisWithError,
    setHashRedis,
    expireRedis,
    getHashKeyRedis,
    deleteKeyRedis,
    deleteKeysRedis,
    incrementKeyRedis,
    getKeyRedisText,
    tryLockRedis,
    unlockRedis,
    lpush,
    rpop,
  )
where

import Beckn.Types.Common
import Beckn.Types.Error (RedisError (..))
import Beckn.Utils.Error.Throwing (fromEitherM)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)

runKV :: forall a m. (HasCallStack, L.MonadFlow m, MonadThrow m, Log m) => L.KVDB a -> m a
runKV = L.runKVDB "redis" >=> fromEitherM RedisError

-- KV
setKeyRedis ::
  ( HasCallStack,
    L.MonadFlow m,
    MonadThrow m,
    Log m,
    A.ToJSON a
  ) =>
  Text ->
  a ->
  m ()
setKeyRedis key val =
  -- TODO: check for "OK" in resp
  void $ runKV $ L.set (DTE.encodeUtf8 key) (BSL.toStrict $ A.encode val)

setExRedis ::
  ( HasCallStack,
    L.MonadFlow m,
    MonadThrow m,
    Log m,
    A.ToJSON a
  ) =>
  Text ->
  a ->
  Int ->
  m ()
setExRedis key value ttl =
  -- TODO: check for "OK" in resp
  void $ runKV $ L.setex (DTE.encodeUtf8 key) (toEnum ttl) (BSL.toStrict $ A.encode value)

getKeyRedis ::
  forall a m.
  ( HasCallStack,
    L.MonadFlow m,
    MonadThrow m,
    Log m,
    A.FromJSON a
  ) =>
  Text ->
  m (Maybe a)
getKeyRedis key = do
  resp <- runKV (L.get (DTE.encodeUtf8 key))
  return $ A.decode . BSL.fromStrict =<< resp

getKeyRedisWithError ::
  ( HasCallStack,
    L.MonadFlow m,
    MonadThrow m,
    Log m,
    A.FromJSON a
  ) =>
  Text ->
  m (Either String a)
getKeyRedisWithError key = do
  resp <- runKV (L.get (DTE.encodeUtf8 key))
  return $
    case resp of
      Just v ->
        maybe (Left $ "decode failed for key: " <> show key) Right $
          A.decode (BSL.fromStrict v)
      Nothing -> Left $ "No Value found for key : " <> show key

setHashRedis ::
  ( HasCallStack,
    L.MonadFlow m,
    MonadThrow m,
    Log m,
    ToJSON a
  ) =>
  Text ->
  Text ->
  a ->
  m ()
setHashRedis key field value =
  -- TODO: check for "OK" in resp
  void $
    runKV $
      L.hset
        (DTE.encodeUtf8 key)
        (DTE.encodeUtf8 field)
        (BSL.toStrict $ A.encode value)

expireRedis ::
  (HasCallStack, L.MonadFlow m, MonadThrow m, Log m) =>
  Text ->
  Int ->
  m ()
expireRedis key ttl =
  void $ runKV $ L.expire (DTE.encodeUtf8 key) (toEnum ttl)

getHashKeyRedis ::
  ( HasCallStack,
    L.MonadFlow m,
    MonadThrow m,
    Log m,
    FromJSON a
  ) =>
  Text ->
  Text ->
  m (Maybe a)
getHashKeyRedis key field = do
  resp <- runKV $ L.hget (DTE.encodeUtf8 key) (DTE.encodeUtf8 field)
  return $ A.decode . BSL.fromStrict =<< resp

deleteKeyRedis ::
  (HasCallStack, L.MonadFlow m, MonadThrow m, Log m) =>
  Text ->
  m Int
deleteKeyRedis = deleteKeysRedis . return

deleteKeysRedis ::
  (HasCallStack, L.MonadFlow m, MonadThrow m, Log m) =>
  [Text] ->
  m Int
deleteKeysRedis rKeys = do
  resp <- runKV $ L.del $ map DTE.encodeUtf8 rKeys
  return $ fromEnum resp

incrementKeyRedis ::
  (HasCallStack, L.MonadFlow m, MonadThrow m, Log m) =>
  Text ->
  m Integer
incrementKeyRedis =
  runKV . L.incr . DTE.encodeUtf8

getKeyRedisText ::
  (HasCallStack, L.MonadFlow m, MonadThrow m, Log m) =>
  Text ->
  m (Maybe Text)
getKeyRedisText key = do
  resp <- runKV (L.get (DTE.encodeUtf8 key))
  return $ DTE.decodeUtf8 <$> resp

tryLockRedis ::
  (HasCallStack, L.MonadFlow m, MonadThrow m, Log m) =>
  Text ->
  Int ->
  m Bool
tryLockRedis key expire = do
  resp <- runKV (L.rawRequest ["SET", buildLockResourceName key, "1", "NX", "EX", maxLockTime])
  case resp of
    Just ("OK" :: ByteString) -> return True
    _ -> return False
  where
    maxLockTime = show expire

unlockRedis ::
  (HasCallStack, L.MonadFlow m, MonadThrow m, Log m) =>
  Text ->
  m ()
unlockRedis key = do
  _ <- deleteKeyRedis $ buildLockResourceName key
  return ()

buildLockResourceName :: (IsString a) => Text -> a
buildLockResourceName key = fromString $ "atlas:locker:" <> DT.unpack key

lpush :: (HasCallStack, L.MonadFlow m, MonadThrow m, Log m, ToJSON a) => Text -> NonEmpty a -> m ()
lpush key list =
  void . runKV @Integer . L.rawRequest $
    ["LPUSH", DTE.encodeUtf8 key] <> map (BSL.toStrict . A.encode) (toList list)

rpop :: (HasCallStack, L.MonadFlow m, MonadThrow m, Log m, FromJSON a) => Text -> m (Maybe a)
rpop key =
  L.runKVDB "redis" (L.rawRequest ["RPOP", DTE.encodeUtf8 key])
    <&> (join . rightToMaybe >=> A.decode . BSL.fromStrict)
