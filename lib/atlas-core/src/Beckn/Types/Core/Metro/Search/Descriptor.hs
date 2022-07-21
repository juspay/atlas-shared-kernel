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

Module      :  Beckn.Types.Core.Metro.Search.Descriptor
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Metro.Search.Descriptor (Descriptor (..), emptyDescriptor) where

import Beckn.Types.App (BaseUrl)
import Beckn.Types.Core.Metro.Search.Image (Image)
import Beckn.Utils.JSON
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data Descriptor = Descriptor
  { name :: Maybe Text,
    code :: Maybe Text,
    symbol :: Maybe Text,
    short_desc :: Maybe Text,
    long_desc :: Maybe Text,
    images :: Maybe [Image],
    audio :: Maybe BaseUrl,
    _3d_render :: Maybe BaseUrl
  }
  deriving (Generic, Show, Eq, ToSchema)

instance FromJSON Descriptor where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Descriptor where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

emptyDescriptor :: Descriptor
emptyDescriptor =
  Descriptor
    { name = Nothing,
      code = Nothing,
      symbol = Nothing,
      short_desc = Nothing,
      long_desc = Nothing,
      images = Nothing,
      audio = Nothing,
      _3d_render = Nothing
    }
