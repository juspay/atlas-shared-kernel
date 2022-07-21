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

Module      :  Beckn.Utils.Error.Throwing

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Error.Throwing
  ( module Beckn.Utils.Error.Throwing,
    IsHTTPException,
  )
where

import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Utils.Logging
import Control.Exception (PatternMatchFail)
import qualified Data.Text as T
import EulerHS.Prelude

throwError :: (HasCallStack, MonadThrow m, Log m, IsBaseException e) => e -> m b
throwError exc = do
  let someExc = toException exc
  logWarning $ makeLogSomeException someExc
  logCallStack
  throwM someExc
  where
    logCallStack =
      withLogTag "CallStack" $
        logDebug . T.pack $ prettyCallStack callStack

fromMaybeM ::
  (HasCallStack, MonadThrow m, Log m, IsBaseException e) => e -> Maybe b -> m b
fromMaybeM err = maybe (throwError err) pure

fromEitherM ::
  (HasCallStack, MonadThrow m, Log m, IsBaseException e) => (left -> e) -> Either left b -> m b
fromEitherM toerr = fromEitherM' (throwError . toerr)

liftEither ::
  (HasCallStack, MonadThrow m, Log m, IsBaseException e) => Either e b -> m b
liftEither = fromEitherM id

fromEitherM' ::
  Applicative m => (l -> m r) -> Either l r -> m r
fromEitherM' f = either f pure

safeCatch ::
  ( HasCallStack,
    MonadCatch m,
    Log m,
    Exception e
  ) =>
  m a ->
  (e -> m a) ->
  m a
safeCatch m f =
  m `catch` \exc ->
    f exc `catch` \(_ :: PatternMatchFail) ->
      throwM exc

-- Given
--   data E = A | B
-- an action
--   throw B `safeCatch` \A -> ...
-- does not fail with "Non-exhaustive patterns"

rethrow ::
  ( HasCallStack,
    MonadCatch m,
    Log m,
    Exception e,
    IsBaseException e'
  ) =>
  m a ->
  (e -> e') ->
  m a
rethrow m f = m `safeCatch` (throwError . f)
