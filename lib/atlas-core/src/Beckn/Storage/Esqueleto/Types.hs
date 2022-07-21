{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


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

Module      :  Beckn.Storage.Esqueleto.Types
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Storage.Esqueleto.Types where

import Data.OpenApi (ToSchema)
import Database.Esqueleto.Experimental
import EulerHS.Prelude hiding (Key)

data Point = Point
  deriving (Generic, Show, Read, Eq, ToSchema)

instance PersistField Point where
  toPersistValue _ = error "This value should not be used in queries directly."
  fromPersistValue _ = return Point

instance PersistFieldSql Point where
  sqlType _ = SqlOther "geography"
