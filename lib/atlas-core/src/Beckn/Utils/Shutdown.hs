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

Module      :  Beckn.Utils.Shutdown
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.Shutdown where

import Beckn.Prelude
import Control.Concurrent.STM.TMVar
import GHC.Conc
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)

type Shutdown = TMVar ()

handleShutdown :: Shutdown -> IO () -> IO () -> IO ()
handleShutdown shutdown onShutdown closeSocket = do
  void $ installHandler sigTERM (Catch $ shutdownAction "sigTERM") Nothing
  void $ installHandler sigINT (Catch $ shutdownAction "sigINT") Nothing
  where
    shutdownAction reason = do
      isLocked <- atomically $ do
        isEmptyTMVar shutdown >>= \case
          True -> do
            putTMVar shutdown ()
            return True
          False -> return False
      when isLocked $ do
        putStrLn ("Shutting down by " <> reason :: Text)
      onShutdown
      closeSocket

waitForShutdown :: Shutdown -> IO ()
waitForShutdown = atomically . takeTMVar

mkShutdown :: IO Shutdown
mkShutdown = newEmptyTMVarIO

untilShutdown ::
  ( MonadIO m,
    MonadReader r m,
    HasField "isShuttingDown" r Shutdown
  ) =>
  m () ->
  m ()
untilShutdown =
  whileM isRunning

isRunning ::
  ( MonadIO m,
    MonadReader r m,
    HasField "isShuttingDown" r Shutdown
  ) =>
  m Bool
isRunning = liftIO . atomically . isEmptyTMVar =<< asks (.isShuttingDown)
