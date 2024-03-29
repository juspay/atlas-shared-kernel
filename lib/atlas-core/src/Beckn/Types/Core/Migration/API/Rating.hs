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

Module      :  Beckn.Types.Core.Migration.API.Rating
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Types.Core.Migration.API.Rating where

import Beckn.Types.Core.Ack
import Beckn.Types.Core.Migration.Feedback (Feedback)
import Beckn.Types.Core.ReqTypes (BecknCallbackReq, BecknReq)
import Data.Aeson (withObject, (.:))
import Data.Aeson.Types (parseFail)
import EulerHS.Prelude hiding (id)
import Servant (JSON, Post, ReqBody, (:>))

type RatingAPI =
  "rating"
    :> ReqBody '[JSON] (BecknReq RatingInfo)
    :> Post '[JSON] AckResponse

ratingAPI :: Proxy RatingAPI
ratingAPI = Proxy

data RatingInfo = RatingInfo
  { id :: Text,
    value :: Int
  }
  deriving (Generic, Show, ToJSON)

instance FromJSON RatingInfo where
  parseJSON = withObject "RatingInfo" $ \obj -> do
    val <- obj .: "value"
    unless (val >= 1 && val <= 5) $ parseFail "Expected value to be from 1 to 5."
    ratedId <- obj .: "id"
    pure $ RatingInfo ratedId val

type OnRatingAPI =
  "on_rating"
    :> ReqBody '[JSON] (BecknCallbackReq FeedbackObject)
    :> Post '[JSON] AckResponse

onRatingAPI :: Proxy OnRatingAPI
onRatingAPI = Proxy

newtype FeedbackObject = FeedbackObject
  { feedback :: Feedback
  }
  deriving (Generic, Show, FromJSON, ToJSON)
