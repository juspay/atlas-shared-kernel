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

Module      :  Beckn.Scheduler.Storage.Queries
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Scheduler.Storage.Queries where

import Beckn.Prelude
import Beckn.Scheduler.Environment
import Beckn.Scheduler.Storage.Tabular
import Beckn.Scheduler.Types
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common (MonadTime (getCurrentTime))
import Beckn.Types.Id
import Beckn.Utils.Common (encodeToText)

create :: JobText -> SqlDB ()
create = create'

findAll :: SchedulerM t [JobText]
findAll = Esq.findAll $ from $ table @JobT

findById :: Id JobText -> SchedulerM t (Maybe JobText)
findById = Esq.findById

getTasksById :: [Id JobText] -> SchedulerM t [JobText]
getTasksById ids = Esq.findAll $ do
  job <- from $ table @JobT
  where_ $ job ^. JobId `in_` valList (map (.getId) ids)
  pure job

getReadyTasks :: (JobTypeConstraints t) => Maybe t -> SchedulerM t [JobText]
getReadyTasks mbType = do
  now <- getCurrentTime
  Esq.findAll $ do
    job <- from $ table @JobT
    where_ $
      job ^. JobStatus ==. val Pending
        &&. job ^. JobScheduledAt <=. val now
        &&. maybe (val True) (\jobType -> job ^. JobJobType ==. val (encodeToText jobType)) mbType
    orderBy [asc $ job ^. JobScheduledAt]
    pure job

updateStatus :: JobStatus -> Id (Job a b) -> SchedulerM t ()
updateStatus newStatus jobId = do
  now <- getCurrentTime
  Esq.update $ \job -> do
    set job [JobStatus =. val newStatus, JobUpdatedAt =. val now]
    where_ $ job ^. JobId ==. val jobId.getId

markAsComplete :: Id (Job a b) -> SchedulerM t ()
markAsComplete = updateStatus Completed

markAsFailed :: Id (Job a b) -> SchedulerM t ()
markAsFailed = updateStatus Failed

updateErrorCountAndFail :: Id (Job a b) -> Int -> SchedulerM t ()
updateErrorCountAndFail jobId fCount = do
  now <- getCurrentTime
  Esq.update $ \job -> do
    set job [JobStatus =. val Failed, JobCurrErrors =. val fCount, JobUpdatedAt =. val now]
    where_ $ job ^. JobId ==. val jobId.getId

reSchedule :: Id (Job a b) -> UTCTime -> SchedulerM t ()
reSchedule jobId newScheduleTime = do
  now <- getCurrentTime
  Esq.update $ \job -> do
    set job [JobScheduledAt =. val newScheduleTime, JobUpdatedAt =. val now]
    where_ $ job ^. JobId ==. val jobId.getId

updateFailureCount :: Id (Job a b) -> Int -> SchedulerM t ()
updateFailureCount jobId newCountValue = do
  now <- getCurrentTime
  Esq.update $ \job -> do
    set job [JobCurrErrors =. val newCountValue, JobUpdatedAt =. val now]
    where_ $ job ^. JobId ==. val jobId.getId

reScheduleOnError :: Id (Job a b) -> Int -> UTCTime -> SchedulerM t ()
reScheduleOnError jobId newCountValue newScheduleTime = do
  now <- getCurrentTime
  Esq.update $ \job -> do
    set
      job
      [ JobScheduledAt =. val newScheduleTime,
        JobUpdatedAt =. val now,
        JobCurrErrors =. val newCountValue
      ]
    where_ $ job ^. JobId ==. val jobId.getId
