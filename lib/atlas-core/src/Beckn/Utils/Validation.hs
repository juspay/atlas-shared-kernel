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

Module      :  Beckn.Utils.Validation

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Validation
  ( module Beckn.Utils.Validation,
    module Beckn.Types.Validation,
  )
where

import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Types.Logging
import Beckn.Types.Predicate
import Beckn.Types.Validation
import Beckn.Utils.Error.Throwing
import qualified Data.Either.Validation as V
import Data.Generics.Labels ()
import EulerHS.Prelude hiding (pred)

runRequestValidation ::
  (MonadThrow m, Log m) =>
  Validate obj ->
  obj ->
  m ()
runRequestValidation validator obj =
  V.validationToEither (validator obj)
    & fromEitherM RequestValidationFailure

newtype RequestValidationFailure = RequestValidationFailure [ValidationDescription]
  deriving (Show, IsBaseError, IsBecknAPIError)

instance IsHTTPError RequestValidationFailure where
  toErrorCode (RequestValidationFailure _failures) = "REQUEST_VALIDATION_FAILURE"
  toHttpCode (RequestValidationFailure _failures) = E400

instance IsAPIError RequestValidationFailure where
  toPayload (RequestValidationFailure failures) = toJSON failures

instanceExceptionWithParent 'HTTPException ''RequestValidationFailure

validateField ::
  (Predicate a p, ShowablePredicate p) =>
  Text ->
  a ->
  p ->
  Validation
validateField fieldName fieldValue pred =
  unless (pFun pred fieldValue) . V.Failure $ [validationDescription]
  where
    validationDescription =
      ValidationDescription
        { fieldName = [fieldName],
          expectation = pShow pred fieldName
        }

validateObject ::
  Text ->
  a ->
  Validate a ->
  Validation
validateObject fieldName object validator = addPrefixes fieldName $ validator object

validateList ::
  Container a =>
  Text ->
  a ->
  Validate (Element a) ->
  Validation
validateList fieldName list validator =
  traverse_ f (zip (map (\i -> fieldName <> "[" <> show i <> "]") [0 :: Int ..]) $ toList list)
  where
    f (pref, val) = addPrefixes pref $ validator val

addPrefixes :: Text -> Validation -> Validation
addPrefixes fieldName = first $ map (addPrefixToFieldName fieldName)

addPrefixToFieldName ::
  Text ->
  ValidationDescription ->
  ValidationDescription
addPrefixToFieldName prefix = #fieldName %~ (prefix :)
