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

Module      :  Beckn.Utils.SlidingWindowLimiter
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.SlidingWindowLimiter where

import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common as Common
import Beckn.Types.Error
import Beckn.Types.SlidingWindowLimiter
import Beckn.Utils.Common hiding (nominalDiffTimeToSeconds)
import Data.Time hiding (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import EulerHS.Prelude hiding (id)
import GHC.Records.Extra

checkSlidingWindowLimit :: HasFlowEnv m r '["apiRateLimitOptions" ::: APIRateLimitOptions] => Text -> m ()
checkSlidingWindowLimit key = do
  hitsLimit <- asks (.apiRateLimitOptions.limit)
  hitsLimitResetTime <- asks (.apiRateLimitOptions.limitResetTimeInSec)
  unlessM (slidingWindowLimiter key hitsLimit hitsLimitResetTime) $
    throwError $ HitsLimitError hitsLimitResetTime

-- Sliding window rate limiter.
-- Returns True if limit is not exceed and further
-- actions should be allowed. False otherwise.

slidingWindowLimiter :: MonadFlow m => Text -> Int -> Int -> m Bool
slidingWindowLimiter key frameHitsLim frameLen = do
  currTime <- getCurrentTime
  hits <- fromMaybe [] <$> Redis.getKeyRedis key
  let (filtHits, ret) = slidingWindowLimiterPure currTime hits frameHitsLim frameLen
  when ret $ Redis.setExRedis key filtHits frameLen
  return ret

slidingWindowLimiterPure :: UTCTime -> [Integer] -> Int -> Int -> ([Integer], Bool)
slidingWindowLimiterPure currTime hits frameHitsLim frameLen = do
  -- How it works:
  -- We convert UTCTime value to Integer and `div` it by frameLen to
  -- get its frame number. After that we getting list with
  -- timeFrames from redis and getting number of calls within
  -- current and previous frame. Getting prevFrameWeight
  -- (timePassedSinceCurrFrameStart/frameLen == 1 >= n >= 0) and
  -- doing check (prevFrameHitsLen * prevFrameWeight + currFrameHitsLen < frameHitsLim).
  -- If passed - add currFrame to frames list, save it in redis and return True. False otherwise.
  let currFrame = getTimeFrame currTime
      filtHits = filter (hitsFilter currFrame) hits
      prevFrameHitsLen = length $ filter (prevFrameHitsFilter currFrame) filtHits
      prevFrameWeight = 1 - (fromIntegral (getTimeWithinFrame currTime) :: Double) / frameLen'
      currFrameHitsLen = length $ filter (currFrameHitsFilter currFrame) filtHits
      res = floor (fromIntegral prevFrameHitsLen * prevFrameWeight) + currFrameHitsLen < frameHitsLim
  (if res then currFrame : filtHits else filtHits, res)
  where
    frameLen' :: Num a => a
    frameLen' = fromIntegral frameLen
    getTimeFrame time = getTime time `div` frameLen'
    getTimeWithinFrame time = getTime time `mod` frameLen'
    getTime :: UTCTime -> Integer
    getTime = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
    hitsFilter currFrame timeFrame = (timeFrame == currFrame - 1) || (timeFrame == currFrame)
    prevFrameHitsFilter currFrame timeFrame = timeFrame == currFrame - 1
    currFrameHitsFilter currFrame timeFrame = timeFrame == currFrame
