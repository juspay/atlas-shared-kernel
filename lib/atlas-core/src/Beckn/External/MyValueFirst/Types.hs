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

Module      :  Beckn.External.MyValueFirst.Types
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.External.MyValueFirst.Types where

import Beckn.Types.Servant
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import EulerHS.Prelude hiding (encodeUtf8, fromStrict, toStrict)
import Servant

data SubmitSms = SubmitSms
  { -- | Login of myfirstvalue.com account.
    username :: Text,
    -- | Password of that account.
    password :: Text,
    -- | Author name assigned to SMS.
    from :: Text,
    -- | Phone number.
    to :: Text,
    -- | SMS contents.
    text :: Text
  }
  deriving (Show)

data SubmitSmsRes
  = Sent
  | BadNumber
  | InvalidReceiver
  | EmptyNumber
  | MissingSender
  | EmptyText
  | UnknownError
  | AuthorizationFailure
  deriving (Generic, FromJSON, ToJSON, Show, Eq)

instance MimeUnrender PlainText_ISO_8859_1 SubmitSmsRes where
  mimeUnrender _ = Right . parseSubmitSmsRes . T.decodeLatin1 . toStrict

instance MimeRender PlainText_ISO_8859_1 SubmitSmsRes where
  mimeRender _ = fromStrict . T.encodeUtf8 . submitSmsResToText

parseSubmitSmsRes :: Text -> SubmitSmsRes
parseSubmitSmsRes txt
  | "Sent." `T.isPrefixOf` txt = Sent
  | "Number" `T.isPrefixOf` txt = BadNumber
  | "Invalid Receiver" `T.isPrefixOf` txt = InvalidReceiver
  | "Empty receiver number" `T.isPrefixOf` txt = EmptyNumber
  | "Sender" `T.isPrefixOf` txt = MissingSender
  | "Empty text not allowed" `T.isPrefixOf` txt = EmptyText
  | "unknown" `T.isPrefixOf` txt = UnknownError
  | "Authorization" `T.isPrefixOf` txt = AuthorizationFailure
  | otherwise = UnknownError

submitSmsResToText :: SubmitSmsRes -> Text
submitSmsResToText = \case
  Sent -> "Sent."
  BadNumber -> "Number(s) has/have been denied by white-and/or black-lists."
  InvalidReceiver -> "Invalid Receiver"
  EmptyNumber -> "Empty receiver number not allowed, rejected"
  MissingSender -> "Sender missing and no global set, rejected"
  EmptyText -> "Empty text not allowed, rejected."
  UnknownError -> "unknown request"
  AuthorizationFailure -> "Authorization failed for sendsms"
