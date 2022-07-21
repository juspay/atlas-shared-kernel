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

Module      :  Beckn.Utils.Common

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Common
  ( module Beckn.Utils.Common,
    module Common,
    callBecknAPI,
  )
where

import Beckn.Prelude as Common (everyPossibleVariant, foldWIndex, identity, show)
import Beckn.Types.Common as Common
import Beckn.Types.Core.Ack as Common
import Beckn.Types.Error.BaseError.HTTPError as Common
import Beckn.Types.Field as Common
import Beckn.Types.Id (ShortId (ShortId))
import Beckn.Utils.Context as Common
import Beckn.Utils.Error as Common
import Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError (callBecknAPI)
import Beckn.Utils.Error.DB as Common
import Beckn.Utils.Logging as Common
import Beckn.Utils.Servant.Client as Common
import Beckn.Utils.Shutdown as Common (Shutdown)
import Beckn.Utils.Text as Common
import Beckn.Utils.Time as Common
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import qualified Test.RandomStrings as RS

generateShortId :: MonadFlow m => m (ShortId a)
generateShortId = ShortId . T.pack <$> liftIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 10)
