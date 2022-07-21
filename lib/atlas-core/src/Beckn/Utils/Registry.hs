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

Module      :  Beckn.Utils.Registry

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Registry
  ( registryFetch,
    Beckn.Utils.Registry.registryLookup,
    whitelisting,
    withSubscriberCache,
  )
where

import Beckn.Prelude
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Cache
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Registry
import qualified Beckn.Types.Registry.API as API
import qualified Beckn.Types.Registry.Routes as Registry
import Beckn.Utils.Common
import Data.Generics.Labels ()
import qualified EulerHS.Types as T

registryLookup ::
  ( CoreMetrics m,
    HasFlowEnv m r '["registryUrl" ::: BaseUrl]
  ) =>
  SimpleLookupRequest ->
  m (Maybe Subscriber)
registryLookup request =
  registryFetch (toLookupReq request)
    >>= \case
      [subscriber] ->
        pure $ Just subscriber
      _subscriber : _subscribers ->
        throwError $ InternalError "Multiple subscribers returned for a unique key."
      [] -> pure Nothing
  where
    toLookupReq SimpleLookupRequest {..} =
      API.emptyLookupRequest
        { API.unique_key_id = Just unique_key_id,
          API.subscriber_id = Just subscriber_id
        }

registryFetch ::
  ( MonadReader r m,
    MonadFlow m,
    CoreMetrics m,
    HasField "registryUrl" r BaseUrl
  ) =>
  API.LookupRequest ->
  m [Subscriber]
registryFetch request = do
  registryUrl <- asks (.registryUrl)
  callAPI registryUrl (T.client Registry.lookupAPI request) "lookup"
    >>= fromEitherM (ExternalAPICallError (Just "REGISTRY_CALL_ERROR") registryUrl)

whitelisting ::
  (MonadThrow m, Log m) =>
  (Text -> m Bool) ->
  Maybe Subscriber ->
  m (Maybe Subscriber)
whitelisting p = maybe (pure Nothing) \sub -> do
  unlessM (p sub.subscriber_id) . throwError . InvalidRequest $
    "Not whitelisted subscriber " <> sub.subscriber_id
  pure (Just sub)

withSubscriberCache ::
  ( MonadTime m,
    CacheEx Subscriber m
  ) =>
  (CacheKey Subscriber -> m (Maybe Subscriber)) ->
  CacheKey Subscriber ->
  m (Maybe Subscriber)
withSubscriberCache getData key = do
  now <- getCurrentTime
  caching (getTtl now) getData key
  where
    getTtl now Subscriber {..} =
      nominalDiffTimeToSeconds . fromMaybe (5 * 60) $ valid_until <&> (`diffUTCTime` now)
