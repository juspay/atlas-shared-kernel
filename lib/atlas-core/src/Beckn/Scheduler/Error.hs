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

Module      :  Beckn.Scheduler.Error
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Scheduler.Error where

import Beckn.Prelude
import Beckn.Types.Error.BaseError
import Beckn.Types.Error.BaseError.HTTPError
import Type.Reflection

data JobDecodeError
  = InvalidJobType Text
  | InvalidJobData Text
  deriving (Show, IsBecknAPIError, Typeable)

instanceExceptionWithParent 'HTTPException ''JobDecodeError

instance IsBaseError JobDecodeError where
  toMessage = \case
    InvalidJobType invalidType -> Just $ "Invalid job type: " <> invalidType
    InvalidJobData invalidData -> Just $ "Invalid job data: " <> invalidData

instance IsHTTPError JobDecodeError where
  toErrorCode = \case
    InvalidJobType _ -> "INVALID_JOB_TYPE"
    InvalidJobData _ -> "INVALID_JOB_DATA"
  toHttpCode _ = E500

instance IsAPIError JobDecodeError
