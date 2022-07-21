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

Module      :  Beckn.Utils.Error.DB
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Error.DB where

import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Utils.Error.Throwing
import EulerHS.Prelude
import qualified EulerHS.Types as ET

throwDBError :: (MonadThrow m, Log m) => ET.DBError -> m a
throwDBError (ET.DBError dbErrType msg) = throwError $
  case dbErrType of
    ET.UnexpectedResult -> SQLResultError msg
    ET.SQLError sqlErr -> SQLRequestError (show sqlErr) msg
    _ -> DBUnknownError msg

checkDBError :: (MonadThrow m, Log m) => ET.DBResult a -> m a
checkDBError = either throwDBError pure

checkDBErrorOrEmpty ::
  (MonadThrow m, Log m, IsBaseException b) =>
  ET.DBResult (Maybe a) ->
  b ->
  m a
checkDBErrorOrEmpty dbres domainErrOnEmpty =
  either throwDBError (fromMaybeM domainErrOnEmpty) dbres
