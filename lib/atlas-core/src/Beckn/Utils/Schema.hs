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

Module      :  Beckn.Utils.Schema

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Schema
  ( stripPrefixUnderscoreIfAny,
    untaggedValue,
    genericDeclareUnNamedSchema,
    objectWithSingleFieldParsing,
  )
where

import Beckn.Prelude
import Beckn.Utils.Common (recursiveStrip)
import qualified Data.Aeson as A
import Data.OpenApi
import Data.OpenApi.Declare
import Data.OpenApi.Internal.Schema
import Data.Typeable
import GHC.Generics

stripPrefixUnderscoreIfAny :: SchemaOptions
stripPrefixUnderscoreIfAny =
  defaultSchemaOptions
    { fieldLabelModifier = recursiveStrip
    }

untaggedValue :: SchemaOptions
untaggedValue =
  defaultSchemaOptions
    { sumEncoding = A.UntaggedValue
    }

genericDeclareUnNamedSchema :: forall a. (Generic a, GToSchema (Rep a), Typeable a) => SchemaOptions -> Proxy a -> Declare (Definitions Schema) NamedSchema
genericDeclareUnNamedSchema opt prx = do
  res <- genericDeclareNamedSchema opt prx
  return $ res {_namedSchemaName = Nothing}

objectWithSingleFieldParsing :: (String -> String) -> SchemaOptions
objectWithSingleFieldParsing constructorMapping =
  defaultSchemaOptions
    { sumEncoding = A.ObjectWithSingleField,
      constructorTagModifier = constructorMapping
    }
