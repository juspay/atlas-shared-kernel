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

Module      :  Beckn.Utils.MVar
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Beckn.Utils.MVar where

import Beckn.Prelude
import qualified Control.Concurrent.MVar as IO

type MVar = IO.MVar

newEmptyMVar :: MonadIO m => m (MVar a)
newEmptyMVar = liftIO IO.newEmptyMVar

newMVar :: MonadIO m => a -> m (MVar a)
newMVar = liftIO . IO.newMVar

takeMVar :: MonadIO m => MVar a -> m a
takeMVar = liftIO . IO.takeMVar

readMVar :: MonadIO m => MVar a -> m a
readMVar = liftIO . IO.readMVar

putMVar :: MonadIO m => MVar a -> a -> m ()
putMVar m = liftIO . IO.putMVar m

tryTakeMVar :: MonadIO m => MVar a -> m (Maybe a)
tryTakeMVar = liftIO . IO.tryTakeMVar

tryPutMVar :: MonadIO m => MVar a -> a -> m Bool
tryPutMVar m = liftIO . IO.tryPutMVar m

tryReadMVar :: MonadIO m => MVar a -> m (Maybe a)
tryReadMVar = liftIO . IO.tryReadMVar

isEmptyMVar :: MonadIO m => MVar a -> m Bool
isEmptyMVar = liftIO . IO.isEmptyMVar

swapMVar :: MonadIO m => MVar a -> a -> m a
swapMVar m = liftIO . IO.swapMVar m

-- TODO: implement MonadMask for FlowR so we can write proper `with` and `modify` functions

modifyMVar' :: MonadIO m => MVar a -> (a -> (a, b)) -> m b
modifyMVar' m f = liftIO . IO.modifyMVar m $ pure . f

modifyMVar_' :: MonadIO m => MVar a -> (a -> a) -> m ()
modifyMVar_' m f = liftIO . IO.modifyMVar_ m $ pure . f
