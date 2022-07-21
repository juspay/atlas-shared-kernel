{-# OPTIONS_GHC -Wno-missing-signatures #-}


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

Module      :  Beckn.Utils.Predicates
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Predicates where

import Beckn.Types.Predicate
import EulerHS.Prelude

digit, latinUC, latinLC, latin, alphanum, latinOrSpace :: Regex
digit = charRange '0' '9'
latinUC = charRange 'A' 'Z'
latinLC = charRange 'a' 'z'
latin = latinUC \/ latinLC
alphanum = latin \/ digit
latinOrSpace = latin \/ " "

mobileNumber = ExactLength 10 `And` star digit

mobileCountryCode = LengthInRange 2 4 `And` ("+" <> star digit)

name = star latinOrSpace
