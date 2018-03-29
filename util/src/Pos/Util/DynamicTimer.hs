-- | Restartable, STM-based dynamic timer build on top of `Pos.Util.Timer.Timer`.
module Pos.Util.DynamicTimer
  ( DynamicTimer
  , newDynamicTimer
  , waitDynamicTimer
  , startDynamicTimer
  ) where

import           Data.Time.Units (Microsecond, toMicroseconds)
import           Control.Concurrent.STM (readTVar, registerDelay, retry)
import           Universum

data DynamicTimer m = DynamicTimer
  { dtDuration  :: !(m Microsecond)
  , dtSemaphore :: !(TVar (TVar Bool))
  }

-- | Construct new `DynamicTimer` from an action that returns time interval
newDynamicTimer :: (MonadIO m, MonadIO d) => m Microsecond -> d (DynamicTimer m)
newDynamicTimer dtDuration =
  DynamicTimer dtDuration <$> (newTVarIO True >>= newTVarIO)

-- | Wait for the duration associated with the DynamicTimer that passed since the last
-- time `startDynamicTimer` was called.
waitDynamicTimer :: DynamicTimer m -> STM ()
waitDynamicTimer DynamicTimer{..} = do
  done <- readTVar =<< readTVar dtSemaphore
  unless done retry

-- | Set the time duration of the underlying timer using the `dtDuration`
-- action and start the underlying timer.
startDynamicTimer :: MonadIO m => DynamicTimer m -> m ()
startDynamicTimer DynamicTimer{..} =
      dtDuration
  >>= liftIO . registerDelay . fromIntegral . toMicroseconds
  >>= atomically . writeTVar dtSemaphore
