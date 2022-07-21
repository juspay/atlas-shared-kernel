{-# LANGUAGE DerivingStrategies #-}
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

Module      :  Beckn.Types.App

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.App
  ( module Beckn.Types.App,
    Servant.BaseUrl,
    Aeson.Value,
  )
where

import Beckn.Types.Field (HasFields)
import Beckn.Types.Forkable
import Beckn.Types.Logging
import Beckn.Types.MonadGuid
import Beckn.Types.Time
import Control.Lens.Operators
import Data.Aeson as Aeson (Value)
import Data.OpenApi
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Servant
import qualified Servant.Client.Core as Servant

data EnvR r = EnvR
  { flowRuntime :: R.FlowRuntime,
    appEnv :: r
  }
  deriving (Generic)

type MonadFlow m =
  ( Monad m,
    MonadIO m,
    L.MonadFlow m,
    Forkable m,
    Log m,
    MonadGuid m,
    MonadTime m,
    MonadClock m,
    MonadThrow m
  )

-- | Require monad to be Flow-based and have specified fields in Reader env.
type HasFlowEnv m r fields =
  ( MonadFlow m,
    MonadReader r m,
    HasFields r fields
  )

type FlowHandlerR r = ReaderT (EnvR r) IO

type FlowServerR r api = ServerT api (FlowHandlerR r)

type MandatoryQueryParam name a = QueryParam' '[Required, Strict] name a

type Limit = Int

type Offset = Int

type RegToken = Text

-- FIXME: remove this
type AuthHeader = Header' '[Required, Strict] "token" RegToken

instance ToSchema Servant.BaseUrl where
  declareNamedSchema _ = do
    aSchema <- declareSchema (Proxy :: Proxy Text)
    return $
      NamedSchema (Just "BaseUrl") aSchema

instance ToSchema Value where
  declareNamedSchema _ = do
    aSchema <- declareSchema (Proxy :: Proxy Text)
    return $
      NamedSchema (Just "Value") $
        aSchema
          & description
            ?~ "Some JSON."

newtype Meter = Meter
  { getDistanceInMeter :: Double
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
