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

Module      :  Beckn.Product.Validation.Context
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Product.Validation.Context where

import Beckn.Types.Common
import qualified Beckn.Types.Core.Context as Cab
import Beckn.Types.Error
import Beckn.Types.Field
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude

validateContext :: (HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]) => Cab.Action -> Cab.Context -> m ()
validateContext action context = do
  validateDomain Cab.MOBILITY context
  validateContextCommons action context

validateDomain :: (L.MonadFlow m, Log m) => Cab.Domain -> Cab.Context -> m ()
validateDomain expectedDomain context =
  unless (context.domain == expectedDomain) $
    throwError InvalidDomain

validateCountry :: (L.MonadFlow m, Log m) => Cab.Context -> m ()
validateCountry context =
  unless (context.country == "IND") $
    throwError InvalidCountry

validateAction :: (L.MonadFlow m, Log m) => Cab.Action -> Cab.Context -> m ()
validateAction expectedAction context =
  unless (context.action == expectedAction) $
    throwError InvalidAction

validateCoreVersion ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    Log m
  ) =>
  Cab.Context ->
  m ()
validateCoreVersion context = do
  supportedVersion <- view #coreVersion
  unless (context.core_version == supportedVersion) $
    throwError UnsupportedCoreVer

validateContextCommons ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    Log m
  ) =>
  Cab.Action ->
  Cab.Context ->
  m ()
validateContextCommons expectedAction context = do
  -- TODO: City validation
  validateAction expectedAction context
  validateCoreVersion context
  validateCountry context
