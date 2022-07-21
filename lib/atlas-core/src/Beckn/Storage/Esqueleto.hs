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

Module      :  Beckn.Storage.Esqueleto

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Storage.Esqueleto
  ( module Types,
    module Functions,
    module Queries,
    module Logger,
    module Config,
    module SqlDB,
    module Class,
    module Reexport,
    module Transactionable,
    defaultQQ,
    defaultSqlSettings,
  )
where

import Beckn.Storage.Esqueleto.Class as Class
import Beckn.Storage.Esqueleto.Config as Config (EsqDBFlow)
import Beckn.Storage.Esqueleto.Functions as Functions
import Beckn.Storage.Esqueleto.Logger as Logger (LoggerIO)
import Beckn.Storage.Esqueleto.Queries as Queries
import Beckn.Storage.Esqueleto.SqlDB as SqlDB
import Beckn.Storage.Esqueleto.Transactionable as Transactionable
import Beckn.Storage.Esqueleto.Types as Types
import Beckn.Utils.Text
import qualified Data.Text as T
import Database.Persist.Quasi.Internal
import Database.Persist.TH as Reexport
import EulerHS.Prelude hiding (Key)
import Language.Haskell.TH.Quote

defaultQQ :: QuasiQuoter
defaultQQ =
  persistWith $
    upperCaseSettings
      { psToDBName = camelCaseToSnakeCase
      }

defaultSqlSettings :: MkPersistSettings
defaultSqlSettings =
  sqlSettings
    { mpsConstraintLabelModifier = \tableName fieldName ->
        if T.last tableName /= 'T'
          then "Table_name_must_end_with_T"
          else T.init tableName <> fieldName,
      mpsFieldLabelModifier = \_ fieldName -> fieldName
    }
