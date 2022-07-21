{-# LANGUAGE QuasiQuotes #-}
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

Module      :  Beckn.Utils.Error.Hierarchy
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Error.Hierarchy where

import Data.Typeable (cast)
import EulerHS.Prelude
import Language.Haskell.TH

instanceExceptionWithParent :: Name -> Name -> DecsQ
instanceExceptionWithParent parent child =
  [d|
    instance Exception $(conT child) where
      toException = toException . $(conE parent)
      fromException = $(pure unPat) <=< fromException
    |]
  where
    unPat =
      let x = mkName "x"
       in LamE [ConP parent [VarP x]] $
            AppE (VarE 'cast) (VarE x)
