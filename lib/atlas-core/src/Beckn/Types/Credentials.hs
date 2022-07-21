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

Module      :  Beckn.Types.Credentials
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Credentials where

import Beckn.Prelude
import Beckn.Types.Base64
import Beckn.Types.Registry.Domain
import Beckn.Types.Registry.Subscriber (SubscriberType)
import Beckn.Utils.Dhall

type PrivateKey = Base64

type PublicKey = Base64

data Credential = Credential
  { shortOrgId :: Text,
    uniqueKeyId :: Text,
    signPubKey :: PublicKey,
    url :: BaseUrl,
    domain :: Domain,
    _type :: SubscriberType
  }
  deriving (Generic, FromDhall)
