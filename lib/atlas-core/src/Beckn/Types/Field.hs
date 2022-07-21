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

Module      :  Beckn.Types.Field
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Field where

import qualified Data.Generics.Labels as GL
import Data.Kind (Constraint, Type)
import GHC.Base (Symbol)
import GHC.Records.Extra (HasField)

-- | An alias for type-level pair of name and type.
type (name :: Symbol) ::: (ty :: Type) = '(name, ty)

-- | Version of 'HasField' which complies with both record-dot-preprocessor
-- and @.field@ syntax supported by generics-lens.
--
-- Re-evaluate this once we decide on a uniform way to access fields.
type HasFieldSuper name r ty = (HasField name r ty, GL.Field name r r ty ty)

-- | Bulk version of @HasField@.
type family HasFields (r :: Type) (fields :: [(Symbol, Type)]) :: Constraint where
  HasFields r '[] = () :: Constraint
  HasFields r ('(name, ty) ': fields) =
    (HasFieldSuper name r ty, HasFields r fields)
