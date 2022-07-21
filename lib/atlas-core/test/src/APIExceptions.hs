{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}


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

Module      :  APIExceptions
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module APIExceptions (httpExceptionTests) where

import Beckn.Tools.Metrics.CoreMetrics
import qualified Beckn.Tools.Metrics.CoreMetrics as Metrics
import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Utils.Error.FlowHandling
import Control.Arrow (left)
import qualified Data.Aeson as A
import EulerHS.Prelude
import qualified Servant as S
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import TestSilentIOLogger ()

data SomeAPIError = SomeAPIError deriving (Show)

instance IsBaseError SomeAPIError

instance IsHTTPError SomeAPIError where
  toErrorCode SomeAPIError = "SOME_API_ERROR"

instance IsAPIError SomeAPIError

instance IsBecknAPIError SomeAPIError

instanceExceptionWithParent 'HTTPException ''SomeAPIError

data SomeBecknAPIError = SomeBecknAPIError deriving (Show)

instance IsBaseError SomeBecknAPIError

instance IsHTTPError SomeBecknAPIError where
  toErrorCode SomeBecknAPIError = "SOME_BECKN_API_ERROR"

instance IsAPIError SomeBecknAPIError

instance IsBecknAPIError SomeBecknAPIError where
  toType SomeBecknAPIError = INTERNAL_ERROR

instanceExceptionWithParent 'HTTPException ''SomeBecknAPIError

instance Metrics.CoreMetrics IO where
  addRequestLatency _ _ _ _ = return ()
  incrementErrorCounter _ _ = return ()
  addUrlCallRetries _ _ = return ()
  addUrlCallRetryFailures _ = return ()

httpExceptionTests :: TestTree
httpExceptionTests =
  testGroup
    "Endpoint exception catchers tests"
    [ testGroup
        "Throwing any error in our endpoints must return HTTPError"
        [ apiErrorInEndpoint,
          becknApiErrorInEndpoint,
          someErrorInEndpoint
        ],
      testGroup
        "Throwing any error in Beckn endpoints must return BecknAPIError"
        [ apiErrorInBecknEndpoint,
          becknApiErrorInBecknEndpoint,
          someErrorInBecknEndpoint
        ]
    ]

apiErrorInEndpoint :: TestTree
apiErrorInEndpoint =
  testCase "Throwing some Domain error" $
    mustThrow @APIError $ apiHandler (throwM SomeAPIError)

becknApiErrorInEndpoint :: TestTree
becknApiErrorInEndpoint =
  testCase "Throwing some Beckn API error" $
    mustThrow @APIError $ apiHandler (throwM SomeBecknAPIError)

someErrorInEndpoint :: TestTree
someErrorInEndpoint =
  testCase "Throwing SomeException" $
    mustThrow @APIError $ apiHandler (error "Some error")

apiErrorInBecknEndpoint :: TestTree
apiErrorInBecknEndpoint =
  testCase "Throwing some Domain error" $
    mustThrow @BecknAPIError $ becknApiHandler (throwM SomeAPIError)

becknApiErrorInBecknEndpoint :: TestTree
becknApiErrorInBecknEndpoint =
  testCase "Throwing some Beckn API error" $
    mustThrow @BecknAPIError $ becknApiHandler (throwM SomeBecknAPIError)

someErrorInBecknEndpoint :: TestTree
someErrorInBecknEndpoint =
  testCase "Throwing SomeException" $
    mustThrow @BecknAPIError $ becknApiHandler (error "Some error")

mustThrow :: forall (e :: Type). (Show e, FromJSON e) => IO () -> IO ()
mustThrow flow = try flow >>= (`shouldSatisfy` isLeft) . serverErrorTo @e

serverErrorTo :: FromJSON a => Either S.ServerError () -> Either a ()
serverErrorTo = left (fromJust . A.decode . S.errBody)
