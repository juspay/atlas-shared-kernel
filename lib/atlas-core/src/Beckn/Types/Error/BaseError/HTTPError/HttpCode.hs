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

Module      :  Beckn.Types.Error.BaseError.HTTPError.HttpCode
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Error.BaseError.HTTPError.HttpCode where

import EulerHS.Prelude
import Servant.Server.Internal

data HttpCode
  = E400
  | E401
  | E402
  | E403
  | E404
  | E429
  | E500
  | E501
  | E503
  deriving (Show)

toServerError :: HttpCode -> ServerError
toServerError = \case
  E400 -> err400
  E401 -> err401
  E402 -> err402
  E403 -> err403
  E404 -> err404
  E429 ->
    ServerError
      { errHTTPCode = 429,
        errReasonPhrase = "Too Many Requests",
        errBody = "",
        errHeaders = []
      }
  E500 -> err500
  E501 -> err501
  E503 -> err503

isInternalError :: HttpCode -> Bool
isInternalError = \case
  E400 -> False
  E401 -> False
  E402 -> False
  E403 -> False
  E404 -> False
  E429 -> False
  E500 -> True
  E501 -> True
  E503 -> True
