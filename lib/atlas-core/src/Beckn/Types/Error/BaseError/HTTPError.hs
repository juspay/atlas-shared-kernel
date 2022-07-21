{-# LANGUAGE TemplateHaskell #-}


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

Module      :  Beckn.Types.Error.BaseError.HTTPError

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Error.BaseError.HTTPError
  ( module Beckn.Types.Error.BaseError.HTTPError,
    module Beckn.Types.Error.BaseError.HTTPError.HttpCode,
    module Beckn.Types.Error.BaseError.HTTPError.APIError,
    module Beckn.Types.Error.BaseError.HTTPError.BecknAPIError,
    module Beckn.Types.Error.BaseError,
    instanceExceptionWithParent,
  )
where

import Beckn.Types.Error.BaseError
import Beckn.Types.Error.BaseError.HTTPError.APIError
import Beckn.Types.Error.BaseError.HTTPError.BecknAPIError
import Beckn.Types.Error.BaseError.HTTPError.HttpCode
import Beckn.Utils.Error.Hierarchy (instanceExceptionWithParent)
import Control.Exception
import EulerHS.Prelude hiding (Show, pack, show)
import Network.HTTP.Types (Header)
import Prelude (Show (..))

type IsHTTPException e = (IsHTTPError e, IsAPIError e, IsBecknAPIError e, Exception e)

class IsBaseError e => IsHTTPError e where
  toErrorCode :: e -> Text

  toHttpCode :: e -> HttpCode
  toHttpCode _ = E500

  toCustomHeaders :: e -> [Header]
  toCustomHeaders _ = []

data HTTPException = forall e. (Exception e, IsHTTPException e) => HTTPException e

instance IsBaseError HTTPException where
  toMessage (HTTPException e) = toMessage e

instance IsHTTPError HTTPException where
  toErrorCode (HTTPException e) = toErrorCode e
  toHttpCode (HTTPException e) = toHttpCode e
  toCustomHeaders (HTTPException e) = toCustomHeaders e

instance Show HTTPException where
  show (HTTPException e) = show e

instanceExceptionWithParent 'BaseException ''HTTPException

toMessageIfNotInternal :: IsHTTPError e => e -> Maybe Text
toMessageIfNotInternal e = if isInternalError (toHttpCode e) then Nothing else toMessage e
