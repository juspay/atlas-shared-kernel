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

Module      :  Beckn.Storage.Esqueleto.Functions

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Storage.Esqueleto.Functions
  ( (<->.),
    getPoint,
    containsPoint,
    IntervalVal (..),
    interval,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Types
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Database.Esqueleto.Internal.Internal

(<->.) :: SqlExpr (Value Point) -> SqlExpr (Value Point) -> SqlExpr (Value Double)
(<->.) = unsafeSqlBinOp " <-> "

getPoint :: (SqlExpr (Value Double), SqlExpr (Value Double)) -> SqlExpr (Value Point)
getPoint (lat, long) = unsafeSqlFunction "ST_SetSRID" (buildSTPoint (long, lat), val (4326 :: Int))

buildSTPoint :: (SqlExpr (Value Double), SqlExpr (Value Double)) -> SqlExpr (Value b)
buildSTPoint = unsafeSqlFunction "ST_Point"

containsPoint :: (Double, Double) -> SqlExpr (Value b)
containsPoint (lon, lat) = unsafeSqlFunction "st_contains" args
  where
    args = (unsafeSqlValue "geom", geomFromText pointText)
    geomFromText = unsafeSqlFunction "ST_GeomFromText"
    pointText = val ("POINT (" <> show lon <> " " <> show lat <> ")") :: SqlExpr (Value Text)

data IntervalVal = YEAR Int | MONTH Int | DAY Int | HOUR Int | MINUTE Int | SECOND Int deriving (Show)

interval :: [IntervalVal] -> SqlExpr (Value UTCTime)
interval intervalVals = unsafeSqlValue valueString
  where
    valueString = "interval '" <> intervalArg <> "'"
    intervalArg = TL.fromLazyText $ TL.unwords (intervalValToString <$> intervalVals)
    intervalValToString = \case
      YEAR i -> show i <> " YEAR"
      MONTH i -> show i <> " MONTH"
      DAY i -> show i <> " DAY"
      HOUR i -> show i <> " HOUR"
      MINUTE i -> show i <> " MINUTE"
      SECOND i -> show i <> " SECOND"
