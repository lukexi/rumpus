{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Clock where
import PreludeExtra

import Rumpus.Systems.Shared
import Rumpus.Systems.PlayPause
import Rumpus.Systems.Physics

type ShouldRepeat = Bool
data ClockAction = ClockAction UTCTime DiffTime ShouldRepeat (EntityMonad ())
defineComponentKey ''ClockAction

initClockSystem :: MonadState ECS m => m ()
initClockSystem = do
    registerComponent "ClockAction" myClockAction (newComponentInterface myClockAction)

tickClockSystem :: ECSMonad ()
tickClockSystem = whenWorldPlaying $ do
    now <- liftIO getCurrentTime
    
    forEntitiesWithComponent myClockAction $ \(entityID, clockAction) -> do
        let ClockAction startTime period shouldRepeat action = clockAction
            elapsed = realToFrac $ now `diffUTCTime` startTime

        when (elapsed > period) $ 
            runEntity entityID $ do
                action
                when shouldRepeat $ 
                    setRepeatingAction period action

setClockAction :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) 
               => DiffTime -> ShouldRepeat -> EntityMonad () -> m ()
setClockAction period shouldRepeat action = do
    startTime <- liftIO getCurrentTime
    myClockAction ==> ClockAction startTime (realToFrac period) shouldRepeat action

setRepeatingAction period action = setClockAction period True action 
setDelayedAction period action = setClockAction period False action 

