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

Module      :  Beckn.Utils.Example
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Example where

import Data.Time
import EulerHS.Prelude

class Example a where
  -- | Sample value of a thing.
  --
  -- This can be used for mocking.
  -- Also, it is especially useful for including examples into swagger,
  -- because random generation can produce non-demostrative values
  -- (e.g. empty lists) unless special care is taken.
  example :: a

instance Example a => Example (Maybe a) where
  example = Just example

instance Example a => Example [a] where
  example = one example

instance Example UTCTime where
  example =
    UTCTime
      (fromGregorian 2020 8 2)
      (timeOfDayToTime (TimeOfDay 7 7 7))

-- until we start using newtypes everywhere
idExample :: Text
idExample = "123e4567-e89b-12d3-a456-426655440000"
