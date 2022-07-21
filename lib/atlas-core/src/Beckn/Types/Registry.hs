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

Module      :  Beckn.Types.Registry

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Registry
  ( module Beckn.Types.Registry,
    module E,
  )
where

import Beckn.Prelude
import Beckn.Types.Registry.Subscriber as E

class Registry m where
  registryLookup :: SimpleLookupRequest -> m (Maybe Subscriber)

data SimpleLookupRequest = SimpleLookupRequest
  { unique_key_id :: Text,
    subscriber_id :: Text
  }
  deriving (Eq, Ord)

lookupRequestToRedisKey :: SimpleLookupRequest -> Text
lookupRequestToRedisKey SimpleLookupRequest {..} = unique_key_id <> "|" <> subscriber_id
