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

Module      :  Beckn.Sms.Config
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Sms.Config where

import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude
import Servant.Client (BaseUrl)

data SmsSessionConfig = SmsSessionConfig
  { attempts :: Int,
    authExpiry :: Int,
    tokenExpiry :: Int
  }
  deriving (Generic, FromDhall)

data SmsCredConfig = SmsCredConfig
  { username :: Text, -- FIXME? Do we need to reuse Servant's one?
    password :: Text, -- idem
    otpHash :: Text
  }
  deriving (Generic, FromDhall)

data SmsConfig = SmsConfig
  { sessionConfig :: SmsSessionConfig,
    credConfig :: SmsCredConfig,
    useFakeSms :: Maybe Word16, -- 4 digit
    url :: BaseUrl,
    sender :: Text
  }
  deriving (Generic, FromDhall)
