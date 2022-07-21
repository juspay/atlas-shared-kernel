{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- Functions rationalToString, validate
-- are only exported for testing purposes.

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

Module      :  Beckn.Types.Core.Taxi.Common.DecimalValue

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Taxi.Common.DecimalValue
  ( DecimalValue (..),
  )
where

import Beckn.Utils.Example
import Control.Lens.Operators
import Data.Char
import Data.OpenApi hiding (Example, value)
import Data.Proxy
import qualified Data.Ratio as R
import qualified Data.Text as T
import EulerHS.Prelude
import qualified Money as M

-- | A type for decimal values based on Rational.
-- Uses "integer.fractional" format for serialization / deserialization.
-- Maximum precision (total number of digits) is defined in this module.
-- Note: serialization of numbers whose integer part has more digits than
-- the maximum precision will fail with an error.
-- Functions / and recip will fail with an error if the denominator is zero.
newtype DecimalValue = DecimalValue Rational
  deriving (Eq, Ord, Show, Read, Generic)
  deriving newtype (Num, Real, Fractional)

instance Example DecimalValue where
  example = DecimalValue 10

maxPrecisionWord8 :: Word8
maxPrecisionWord8 = 30

maxPrecision :: Int
maxPrecision = fromIntegral maxPrecisionWord8

message :: Text
message =
  "Maximum allowed precision (total number of digits) is "
    <> show maxPrecision

-- Functions rationalToString and valueToString should only be used
-- for serialization. They don't perform any proper rounding and simply
-- stop generating digits when at the maximum precision.
-- Note: rationalToString will return Nothing if the integer
-- part of the number exceeds the precision (total number of digits).
rationalToString :: Int -> Rational -> Maybe String
rationalToString precision rational
  | length intPart > precision = Nothing
  | otherwise = Just result
  where
    result =
      if fracPrecision <= 0 || null fracPart
        then intPart
        else intPart <> "." <> take fracPrecision fracPart
    rNumerator = R.numerator rational
    rDenominator = R.denominator rational
    sign = if rNumerator < 0 then "-" else ""
    intPart = sign <> show quotient
    fracPrecision = precision - length intPart
    fracPart = expand remainder
    (quotient, remainder) = abs rNumerator `quotRem` rDenominator
    expand currentRemainder
      | currentRemainder == 0 = ""
      | otherwise = show digit <> expand nextRemainder
      where
        (digit, nextRemainder) = (10 * currentRemainder) `quotRem` rDenominator

-- Note: valueToString will fail with an error if the integer
-- part of the number exceeds the precision (total number of digits).
valueToString :: DecimalValue -> Text
valueToString value =
  maybe
    (error ("Cannot convert " <> show value <> " to a string. " <> message))
    T.pack
    (rationalToString maxPrecision (toRational value))

valueFromString :: Text -> Maybe DecimalValue
valueFromString valueString =
  DecimalValue . toRational <$> M.denseFromDecimal decimalConf valueString
  where
    -- The exact value passed in DecimalConf.decimalConf_digits to
    -- denseFromDecimal does not matter, but it should be large enough to
    -- make sure there is no precision loss.
    decimalConf =
      M.defaultDecimalConf
        { M.decimalConf_digits = maxPrecisionWord8
        }

validate :: Int -> Text -> Bool
validate precision valueString =
  T.length valueString <= maxPossibleLength
    && countDigits valueString <= precision
  where
    -- Combined length of "-" and "."
    maxNonDigitLength = 2
    maxPossibleLength = T.length valueString + maxNonDigitLength
    countDigits = T.length . T.filter isDigit

-- Note: toJSON will fail with an error if the integer
-- part of the number exceeds the precision (total number of digits).
instance ToJSON DecimalValue where
  toJSON = toJSON . valueToString

instance FromJSON DecimalValue where
  parseJSON value = do
    valueString <- parseJSON value
    unless (validate maxPrecision valueString) $ failText message
    maybe (parseError valueString) pure $ valueFromString valueString
    where
      parseError valueString =
        failText $ "Cannot parse " <> valueString <> " as a DecimalValue."
      failText = fail . T.unpack

instance ToSchema DecimalValue where
  declareNamedSchema _ = do
    aSchema <- declareSchema (Proxy :: Proxy Text)
    return $
      NamedSchema (Just "DecimalValue") $
        aSchema
          & description
            ?~ "Decimal value in a string representation \
               \with an optional leading \"-\" for negative numbers. \
               \Integer and fractional parts are separated with a dot."
              <> message
              <> " String format is used to prevent loss of precision."
          & format ?~ "[-]?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?"
