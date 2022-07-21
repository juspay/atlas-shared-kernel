{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


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

Module      :  Beckn.Types.Id
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Id where

import Beckn.Types.GuidLike
import Beckn.Types.MonadGuid
import Beckn.Utils.Example (Example (..), idExample)
import Beckn.Utils.GenericPretty
import Data.OpenApi (ToParamSchema, ToSchema)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple.FromField (FromField)
import Dhall
import EulerHS.Prelude
import Servant (FromHttpApiData (parseUrlPiece), ToHttpApiData)

newtype Id domain = Id
  {getId :: Text}
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData, ToSchema, ToParamSchema, FromField, PrettyShow)

cast :: Id a -> Id b
cast = Id . getId

instance Example (Id a) where
  example = Id idExample

instance IsString (Id d) where
  fromString = Id . Text.pack

instance FromHttpApiData (Id a) where
  parseUrlPiece = pure . Id

instance (MonadGuid m) => GuidLike m (Id a) where
  generateGUID = Id <$> generateGUIDText

newtype ShortId domain = ShortId
  { getShortId :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData, ToSchema, PrettyShow)

instance FromDhall (ShortId a) where
  autoWith _ = ShortId <$> strictText

instance IsString (ShortId d) where
  fromString = ShortId . Text.pack

instance FromHttpApiData (ShortId a) where
  parseUrlPiece = pure . ShortId

instance (MonadGuid m) => GuidLike m (ShortId a) where
  generateGUID = ShortId <$> generateGUIDText
