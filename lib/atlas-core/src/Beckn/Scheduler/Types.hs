{-# LANGUAGE DerivingVia #-}


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

Module      :  Beckn.Scheduler.Types
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Scheduler.Types where

import Beckn.Prelude
import Beckn.Scheduler.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.GenericPretty

-- Job initializer
-- (here one can think of discarding outdated jobs,
-- using maximumDelay :: (Maybe Int) field)
data JobEntry t d = JobEntry
  { jobType :: t,
    jobData :: d,
    maxErrors :: Int
  }
  deriving (Show, Generic, PrettyShow)

type JobEntryText = JobEntry Text Text

-- Main datatype
data Job t d = Job
  { id :: Id (Job t d),
    jobType :: t, -- user defined, one per server
    jobData :: d, -- user defined, one per job handler
    scheduledAt :: UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    maxErrors :: Int,
    currErrors :: Int,
    status :: JobStatus
  }
  deriving (Eq, Show, Generic, PrettyShow)

setJobType :: t2 -> Job t1 d -> Job t2 d
setJobType type2_ Job {..} = Job {id = cast id, jobType = type2_, ..}

setJobData :: d2 -> Job t d1 -> Job t d2
setJobData data2_ Job {..} = Job {id = cast id, jobData = data2_, ..}

type JobTypeConstraints a = (Eq a, Ord a, Show a, FromJSON a, ToJSON a)

type JobDataConstraints a = (Show a, FromJSON a, ToJSON a)

encodeJob :: (JobTypeConstraints a, JobDataConstraints b) => Job a b -> JobText
encodeJob Job {..} =
  Job
    { id = cast id,
      jobType = encodeToText jobType,
      jobData = encodeToText jobData,
      ..
    }

decodeJob :: forall a b. (JobTypeConstraints a, JobDataConstraints b) => JobText -> Either JobDecodeError (Job a b)
decodeJob Job {..} = do
  jobType_ <- maybe (Left $ InvalidJobType jobType) Right $ decodeFromText jobType
  jobData_ <- maybe (Left $ InvalidJobData jobData) Right $ decodeFromText jobData
  pure
    Job
      { id = cast id,
        jobType = jobType_,
        jobData = jobData_,
        ..
      }

type JobText = Job Text Text

data JobStatus = Pending | Completed | Failed
  deriving (Show, Eq, Read, Generic)
  deriving (PrettyShow) via Showable JobStatus

data ExecutionResult = Complete | Terminate Text | Retry | ReSchedule UTCTime
  deriving (Show, Generic, Exception)
  deriving (PrettyShow) via Showable ExecutionResult
