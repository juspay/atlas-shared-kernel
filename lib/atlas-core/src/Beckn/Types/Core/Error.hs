{-# LANGUAGE DerivingVia #-}


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

Module      :  Beckn.Types.Core.Error
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Error where

import Beckn.Utils.GenericPretty (PrettyShow, Showable (Showable))
import Beckn.Utils.JSON
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data Error = Error
  { _type :: ErrorType,
    code :: Text,
    path :: Maybe Text,
    message :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToSchema, PrettyShow)

data ErrorType
  = CONTEXT_ERROR
  | CORE_ERROR
  | INTERNAL_ERROR -- Not a spec value. TODO: get rid of it.
  | DOMAIN_ERROR
  | POLICY_ERROR
  | JSON_SCHEMA_ERROR
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSchema)
  deriving (PrettyShow) via Showable ErrorType

instance FromJSON ErrorType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON ErrorType where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON Error where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Error where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
