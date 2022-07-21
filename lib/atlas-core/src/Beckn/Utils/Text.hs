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

Module      :  Beckn.Utils.Text
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Text where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import EulerHS.Prelude

compareWithoutRightSpaces :: Text -> Text -> Bool
compareWithoutRightSpaces = (==) `on` T.dropWhileEnd Char.isSpace

decodeFromText :: FromJSON a => Text -> Maybe a
decodeFromText = A.decode . BSL.fromStrict . DT.encodeUtf8

encodeToText :: ToJSON a => a -> Text
encodeToText = DT.decodeUtf8 . BSL.toStrict . A.encode

padLeft :: Int -> Char -> Text -> Text
padLeft n c txt = T.replicate (max 0 $ n - length txt) (T.singleton c) <> txt

-- Suits only for non-negative numbers
padNumber :: Integral i => Int -> i -> Text
padNumber n num = padLeft n '0' $ show (fromIntegral num :: Natural)

recursiveStrip :: String -> String
recursiveStrip = \case
  ('_' : xs) -> recursiveStrip xs
  a -> a

maskText :: Text -> Text
maskText text =
  if length text > 6
    then T.take 3 text <> "..." <> T.takeEnd 3 text
    else "..."

camelCaseToSnakeCase :: Text -> Text
camelCaseToSnakeCase =
  T.concatMap \c ->
    if Char.isUpper c then T.pack ['_', Char.toLower c] else T.singleton c
