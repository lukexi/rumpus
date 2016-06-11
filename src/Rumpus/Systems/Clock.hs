module Rumpus.Systems.Clock where
import PreludeExtra

--import Rumpus.Systems.PlayPause

type ShouldRepeat = Bool
data ClockAction = ClockAction UTCTime DiffTime ShouldRepeat (EntityMonad ())
defineComponentKey ''ClockAction

initClockSystem :: MonadState ECS m => m ()
initClockSystem = do
    registerComponent "ClockAction" myClockAction (newComponentInterface myClockAction)

tickClockSystem :: ECSMonad ()
tickClockSystem = do
    now <- liftIO getCurrentTime

    forEntitiesWithComponent myClockAction $ \(entityID, clockAction) -> do
        let ClockAction startTime period shouldRepeat action = clockAction
            elapsed = realToFrac $ now `diffUTCTime` startTime

        when (elapsed > period) $
            inEntity entityID $ do
                if shouldRepeat
                    then setRepeatingAction period action
                    else removeComponent myClockAction
                action


setClockAction :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
               => DiffTime -> ShouldRepeat -> EntityMonad () -> m ()
setClockAction period shouldRepeat action = do
    startTime <- liftIO getCurrentTime
    myClockAction ==> ClockAction startTime (realToFrac period) shouldRepeat action

setRepeatingAction :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => DiffTime -> EntityMonad () -> m ()
setRepeatingAction period action = setClockAction period True action

setDelayedAction :: (MonadIO m, MonadState ECS m, MonadReader EntityID m) => DiffTime -> EntityMonad () -> m ()
setDelayedAction period action = setClockAction period False action

