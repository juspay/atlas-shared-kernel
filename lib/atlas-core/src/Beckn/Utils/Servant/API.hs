{-# LANGUAGE PolyKinds #-}


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

Module      :  Beckn.Utils.Servant.API

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Servant.API
  ( type (:>|),
  )
where

import Data.Kind (Type)
import Servant

-- | This behaves similarly to ':>' from API point of view, but in implementation
-- it attaches a parameter to /each/ separate endpoint, not /all/ of them at once.
--
-- For instance:
-- @
-- type API = Header "auth" Text :> (Get '[JSON] Text :<|> Post '[JSON] ())
-- @
--
-- requires the following implementation:
--
-- @
-- handlers :: Server API
-- handlers = \auth -> get auth :<|> new auth
-- @
--
-- But when ':>' is replaced with ':>|', you can write just
--
-- @
-- handlers = get :<|> auth
-- @
--
-- Note that ':>|' has fewer priority that ':<|>' so you can omit parentheses.
--
-- This operator is experimental, if you find ':>' more appropriate then use it.
type family (:>|) (pre :: k) (api :: Type) where
  pre :>| (api1 :<|> api2) = (pre :>| api1) :<|> (pre :>| api2)
  pre :>| api = pre :> api

infixr 2 :>|
