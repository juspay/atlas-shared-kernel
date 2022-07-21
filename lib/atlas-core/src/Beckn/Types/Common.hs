{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

Module      :  Beckn.Types.Common

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Common
  ( module Beckn.Types.Common,
    module Common,
    HasField,
  )
where

import Beckn.External.Encryption as Common (EncFlow)
import Beckn.External.FCM.Types as Common (FCMFlow)
import Beckn.Storage.DB.Config as Common (DBFlow)
import Beckn.Storage.Esqueleto.Config as Common (EsqDBFlow)
import Beckn.Types.App as Common
import Beckn.Types.Forkable as Common
import Beckn.Types.GuidLike as Common
import Beckn.Types.Logging as Common
import Beckn.Types.MonadGuid as Common
import Beckn.Types.Time as Common
import Beckn.Utils.Dhall (FromDhall)
import Data.Aeson
import Data.Generics.Labels ()
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import GHC.Records.Extra (HasField)

newtype IdObject = IdObject
  { id :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

class FromBeckn a b where
  fromBeckn :: a -> b

class ToBeckn a b where
  toBeckn :: b -> a

-- This type class is not strictly an isomorphism. We use the name 'Iso' to
-- denote that it is always expected that a type from the beckn spec should
-- have a corresponding type defined by us allowing conversion (which may be
-- lossy) between the two, when defined as an instance of this typeclass.
class (FromBeckn a b, ToBeckn a b) => BecknSpecIso a b

newtype Meters = Meters
  { getMeters :: Int
  }
  deriving newtype (Show, Num, FromDhall, FromJSON, ToJSON, Integral, Real, Ord, Eq, Enum)
  deriving stock (Generic)

newtype Kilometers = Kilometers
  { getKilometers :: Double
  }
  deriving newtype (Show, Num, FromDhall, FromJSON, ToJSON, Fractional, Real, Ord, Eq, Enum)
  deriving stock (Generic)
