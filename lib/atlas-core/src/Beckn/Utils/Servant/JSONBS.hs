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

Module      :  Beckn.Utils.Servant.JSONBS
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Servant.JSONBS where

import qualified Data.ByteString.Lazy as BS
import qualified Data.List.NonEmpty as NE
import EulerHS.Prelude
import qualified Network.HTTP.Media as M
import Servant

data JSONBS deriving (Typeable)

instance Accept JSONBS where
  contentTypes _ =
    "application" M.// "json" M./: ("charset", "utf-8")
      NE.:| ["application" M.// "json"]

instance MimeRender JSONBS ByteString where
  mimeRender _ = BS.fromStrict

instance MimeUnrender JSONBS ByteString where
  mimeUnrender _ = pure . BS.toStrict
