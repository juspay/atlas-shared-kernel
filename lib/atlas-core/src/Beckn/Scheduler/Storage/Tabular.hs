{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}


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

Module      :  Beckn.Scheduler.Storage.Tabular
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Scheduler.Storage.Tabular where

import Beckn.Prelude
import qualified Beckn.Scheduler.Types as ST
import Beckn.Storage.Esqueleto
import Beckn.Types.Id

derivePersistField "ST.JobStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    JobT sql=job
      id Text
      jobType Text
      jobData Text
      scheduledAt UTCTime
      createdAt UTCTime
      updatedAt UTCTime
      maxErrors Int
      currErrors Int
      status ST.JobStatus
      Primary id
      deriving Generic
    |]

instance TEntityKey JobT where
  type DomainKey JobT = Id (ST.Job Text Text)
  fromKey (JobTKey _id) = Id _id
  toKey (Id id) = JobTKey id

instance TEntity JobT ST.JobText where
  fromTEntity jobEntity = do
    let JobT {..} = entityVal jobEntity
    pure $
      ST.Job
        { id = Id id,
          ..
        }
  toTType ST.Job {..} =
    JobT
      { id = getId id,
        ..
      }
  toTEntity job = Entity (toKey job.id) $ toTType job
