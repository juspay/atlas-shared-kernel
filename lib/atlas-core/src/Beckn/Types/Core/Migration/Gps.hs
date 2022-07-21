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

Module      :  Beckn.Types.Core.Migration.Gps
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Migration.Gps (Gps (..)) where

import Beckn.Utils.Error.Throwing (fromEitherM')
import Beckn.Utils.Example
import Beckn.Utils.GenericPretty (PrettyShow)
import Control.Arrow ((>>>))
import Data.Aeson
import Data.Aeson.Types (parseFail)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import EulerHS.Prelude hiding (many, try, (<|>))
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

-- Regular expression: ^[-+]?([1-8]?\d(\.\d+)?|90(\.0+)?),\s*[-+]?(180(\.0+)?|((1[0-7]\d)|([1-9]?\d))(\.\d+)?)$

data Gps = Gps
  { lat :: Double,
    lon :: Double
  }
  deriving (Generic, Show, ToSchema, PrettyShow, Eq)

instance Example Gps where
  example =
    Gps
      { lat = 20.5937,
        lon = 78.9629
      }

instance FromJSON Gps where
  parseJSON =
    withText "Gps" $
      T.unpack
        >>> parse parseGps ""
        >>> fromEitherM' (parseFail . show)

instance ToJSON Gps where
  toJSON (Gps lat lon) = String $ show lat <> ", " <> show lon

parseGps :: Parser Gps
parseGps =
  Gps
    <$> (double >>= validate ((<= 90.0) . abs))
    <* char ','
    <* spaces
    <*> (double >>= validate ((<= 180.0) . abs))
    <* eof

type Parser = Parsec String ()

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser emptyDef

double :: Parser Double
double = P.float lexer

validate :: Show a => (a -> Bool) -> a -> Parser a
validate p a = if p a then pure a else unexpected (show a)
