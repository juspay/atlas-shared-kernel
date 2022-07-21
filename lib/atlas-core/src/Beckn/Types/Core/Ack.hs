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

Module      :  Beckn.Types.Core.Ack
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Ack where

import Data.Aeson
import Data.Aeson.Types (unexpected)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding ((.=))

data AckResponse = Ack
  deriving (Generic, Show, ToSchema)

instance FromJSON AckResponse where
  parseJSON = withObject "Ack" $ \v -> do
    status <-
      (v .: "message")
        >>= (.: "ack")
        >>= (.: "status")
    unless (status == String "ACK") (unexpected status)
    pure Ack

instance ToJSON AckResponse where
  toJSON Ack = "message" .== "ack" .== "status" .== String "ACK"
    where
      (.==) :: Text -> Value -> Value
      k .== v = Object (k .= v)
      infixr 9 .==
