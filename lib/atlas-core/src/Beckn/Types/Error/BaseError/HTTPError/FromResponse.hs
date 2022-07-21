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

Module      :  Beckn.Types.Error.BaseError.HTTPError.FromResponse
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Error.BaseError.HTTPError.FromResponse where

import qualified Data.Aeson as A
import EulerHS.Prelude
import Servant.Client (Response, ResponseF (Response))

class FromResponse e where
  fromResponse :: Response -> Maybe e

fromJsonResponse :: FromJSON a => Response -> Maybe a
fromJsonResponse (Response _ _ _ body) = A.decode body
