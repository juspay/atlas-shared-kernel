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

Module      :  Beckn.Scheduler.App

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Scheduler.App
  ( runScheduler,
    createJobByTime,
    createJobIn,
  )
where

import Beckn.Prelude
import Beckn.Scheduler.Environment
import Beckn.Scheduler.JobHandler
import Beckn.Scheduler.Metrics
import Beckn.Scheduler.Storage.Queries
import qualified Beckn.Scheduler.Storage.Queries as Q
import Beckn.Scheduler.Types
import Beckn.Storage.Esqueleto
import Beckn.Storage.Esqueleto.Config (prepareEsqDBEnv)
import Beckn.Storage.Hedis (connectHedis)
import qualified Beckn.Storage.Hedis.Queries as Hedis
import qualified Beckn.Tools.Metrics.Init as Metrics
import Beckn.Types.Common
import Beckn.Types.Error (GenericError (InternalError))
import Beckn.Types.Id
import Beckn.Utils.App
import Beckn.Utils.Common
import Beckn.Utils.IOLogging (prepareLoggerEnv)
import Beckn.Utils.Servant.Server
import Beckn.Utils.Shutdown
import qualified Control.Monad.Catch as C
import Control.Monad.Trans.Cont
import qualified Data.Map as Map
import Servant (Context (EmptyContext))
import System.Exit
import UnliftIO

runScheduler ::
  forall t.
  JobTypeConstraints t =>
  SchedulerConfig t ->
  JobHandlerList t ->
  IO ()
runScheduler SchedulerConfig {..} handlersList = do
  hostname <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  hedisEnv <- connectHedis hedisCfg (\k -> hedisPrefix <> ":" <> k)
  metrics <- setupSchedulerMetrics
  isShuttingDown <- mkShutdown
  let handlersMap = Map.fromList handlersList

  let schedulerEnv = SchedulerEnv {..}
  when (tasksPerIteration <= 0) $ do
    hPutStrLn stderr ("tasksPerIteration should be greater than 0" :: Text)
    exitFailure

  Metrics.serve metricsPort
  let serverStartAction = runner
  withAsync (runSchedulerM schedulerEnv serverStartAction) $ \schedulerAction ->
    runServerGeneric
      schedulerEnv
      (Proxy @HealthCheckAPI)
      healthCheck
      identity
      identity
      EmptyContext
      (const identity)
      (\_ -> cancel schedulerAction)
      runSchedulerM

-- TODO: explore what is going on here
-- To which thread do signals come?

runner :: (JobTypeConstraints t) => SchedulerM t ()
runner = do
  before <- getCurrentTime
  runnerIteration
  after <- getCurrentTime
  let diff = floor $ abs $ diffUTCTime after before
  loopIntervalSec <- asks (.loopIntervalSec)
  threadDelaySec (loopIntervalSec - diff)
  runner

errorLogger :: (Log m, Show a) => a -> m ()
errorLogger e = logError $ "error occured: " <> show e

runnerIteration :: (JobTypeConstraints t) => SchedulerM t ()
runnerIteration = do
  jobType <- asks (.jobType)
  readyTasks <- getReadyTasks jobType
  tasksPerIteration <- asks (.tasksPerIteration)
  availableReadyTasksIds <- pickTasks tasksPerIteration $ map (.id) readyTasks
  logPretty DEBUG "available tasks" availableReadyTasksIds
  takenTasksUpdatedInfo <- getTasksById availableReadyTasksIds
  terminationMVar <- newEmptyMVar
  let inspectTermination = modifyMVarMasked_ terminationMVar pure
      waitAll :: MonadUnliftIO m => [Async a] -> m ()
      waitAll = mapConcurrently_ waitCatch
  flip withAsync (waitEitherTerminationOrExecEnd terminationMVar) $
    withAsyncList (map runTask takenTasksUpdatedInfo) $ \asyncList -> do
      res <- race (waitAll asyncList) inspectTermination
      case res of
        Left _ -> pure ()
        Right _ -> do
          mapM_ cancel asyncList
          waitAll asyncList
  where
    waitEitherTerminationOrExecEnd :: MVar () -> Async () -> SchedulerM t ()
    waitEitherTerminationOrExecEnd termMVar exec =
      void (waitCatch exec) `C.catchAll` \e -> mask $ \restore -> do
        logInfo "terminating gracefully"
        errorLogger e
        termPeriod <- asks (.graceTerminationPeriod)
        restore (threadDelaySec termPeriod) `C.catchAll` \e' ->
          logInfo "terminating immediately" >> errorLogger e'
        putMVar termMVar ()
        throwIO e

    withLogTag' job = withLogTag ("JobId=" <> job.id.getId)

    runTask job = mask $ \restore -> withLogTag' job $ do
      res <- measuringDuration registerDuration $ restore (executeTask job) `C.catchAll` defaultCatcher
      registerExecutionResult job res
      releaseLock job.id

    pickTasks :: Int -> [Id JobText] -> SchedulerM t [Id JobText]
    pickTasks _ [] = pure []
    pickTasks 0 _ = pure []
    pickTasks tasksRemain (x : xs) = do
      gainedLock <- attemptTaskLockAtomic x
      if gainedLock
        then (x :) <$> pickTasks (tasksRemain - 1) xs
        else pickTasks tasksRemain xs

withAsyncList :: MonadUnliftIO m => [m a] -> ([Async a] -> m b) -> m b
withAsyncList actions func =
  flip runCont func $ traverse (cont . withAsync) actions

registerDuration :: Milliseconds -> a -> SchedulerM t ()
registerDuration millis _ = do
  let durSecDouble = millisToSecondsDouble millis
  observeJobExecDuration durSecDouble
  logInfo $ "job execution took " <> show (realToFrac @_ @NominalDiffTime durSecDouble)

-- TODO: refactor the prometheus metrics to measure data that we really need

attemptTaskLockAtomic :: Id (Job a b) -> SchedulerM t Bool
attemptTaskLockAtomic jobId = do
  expirationTime <- asks (.expirationTime)
  Hedis.setNxExpire jobId.getId expirationTime ()

-- TODO: refactor this function so that there was no duplication with the `tryLockRedis` function

releaseLock :: Id (Job a b) -> SchedulerM t ()
releaseLock jobId = Hedis.del jobId.getId

-- TODO: think about more robust style of working with redis locks
-- see https://redis.io/docs/reference/patterns/distributed-locks/

logFailJob :: JobText -> Text -> SchedulerM t ()
logFailJob jobText description = do
  logError $ "failed to execute job: " <> description
  logPretty ERROR "failed job" jobText
  markAsFailed jobText.id

withJobDataDecoded ::
  forall d t m.
  (JobDataConstraints d, Log m, Monad m) =>
  Job t Text ->
  (Job t d -> m ExecutionResult) ->
  m ExecutionResult
withJobDataDecoded txtDataJob action =
  maybe errHandler successHandler $ decodeFromText @d txtDataJob.jobData
  where
    errHandler = do
      let description = "failed to decode job data: " <> txtDataJob.jobData
      logError description
      pure $ Terminate description
    successHandler jobData_ = action $ setJobData jobData_ txtDataJob

executeTask :: forall t. (JobTypeConstraints t) => JobText -> SchedulerM t ExecutionResult
executeTask rawJob = do
  let eithDecodedType = decodeFromText @t rawJob.jobType
  case eithDecodedType of
    Nothing -> do
      let description = "type decode failure: " <> rawJob.jobType
      logFailJob rawJob description
      pure $ Terminate description
    Just decJobType -> do
      hMap <- asks (.handlersMap)
      case Map.lookup decJobType hMap of
        Nothing -> do
          let description = "no handler function found for the job type = " <> show decJobType
          logFailJob rawJob description
          pure $ Terminate description
        Just (JobHandler handlerFunc_) -> do
          let job = setJobType decJobType rawJob
          withJobDataDecoded job $ liftIO . handlerFunc_

registerExecutionResult :: Job t0 Text -> ExecutionResult -> SchedulerM t ()
registerExecutionResult job result =
  case result of
    Complete -> do
      logInfo $ "job successfully completed on try " <> show (job.currErrors + 1)
      markAsComplete job.id
    Terminate description -> do
      logInfo $ "job terminated on try " <> show (job.currErrors + 1) <> "; reason: " <> description
      markAsFailed job.id
    ReSchedule reScheduledTime -> do
      logInfo $ "job rescheduled on time = " <> show reScheduledTime
      reSchedule job.id reScheduledTime
    Retry ->
      let newErrorsCount = job.currErrors + 1
       in if newErrorsCount >= job.maxErrors
            then do
              logError $ "retries amount exceeded, job failed after try " <> show newErrorsCount
              updateErrorCountAndFail job.id newErrorsCount
            else do
              logInfo $ "try " <> show newErrorsCount <> " was not successful, trying again"
              waitBeforeRetry <- asks (.waitBeforeRetry)
              now <- getCurrentTime
              reScheduleOnError job.id newErrorsCount $
                fromIntegral waitBeforeRetry `addUTCTime` now

defaultCatcher :: C.MonadThrow m => SomeException -> m ExecutionResult
defaultCatcher _ = pure defaultResult

defaultResult :: ExecutionResult
defaultResult = Terminate "uncaught exception"

-- api

type SchedulingConstraints r t d =
  ( JobTypeConstraints t,
    JobDataConstraints d,
    Show t,
    Show d
  )

createJobIn ::
  SchedulingConstraints r t d =>
  NominalDiffTime ->
  JobEntry t d ->
  SqlDB (Id (Job t d))
createJobIn diff jobEntry = do
  now <- getCurrentTime
  when (diff < 0) $ throwError $ InternalError "job can only be scheduled for now or for future"
  let scheduledAt = addUTCTime diff now
  createJob scheduledAt jobEntry

createJobByTime ::
  SchedulingConstraints r t d =>
  UTCTime ->
  JobEntry t d ->
  SqlDB (Id (Job t d))
createJobByTime scheduledAt jobEntry = do
  now <- getCurrentTime
  when (scheduledAt <= now) $
    throwError $
      InternalError
        "job can only be scheduled for the future\
        \ using createJobByTime, for scheduling for\
        \ now use createJobIn function instead"
  createJob scheduledAt jobEntry

createJob ::
  forall t d r.
  SchedulingConstraints r t d =>
  UTCTime ->
  JobEntry t d ->
  SqlDB (Id (Job t d))
createJob scheduledAt jobEntry = do
  when (jobEntry.maxErrors <= 0) $ throwError $ InternalError "maximum errors should be positive"
  now <- getCurrentTime
  id <- Id <$> generateGUIDText
  let job = makeJob id now
      jobText = encodeJob job
  Q.create jobText
  pure job.id
  where
    makeJob id currentTime =
      Job
        { id = id,
          jobType = jobEntry.jobType,
          jobData = jobEntry.jobData,
          scheduledAt = scheduledAt,
          maxErrors = jobEntry.maxErrors,
          createdAt = currentTime,
          updatedAt = currentTime,
          currErrors = 0,
          status = Pending
        }
