module Rumpus.Systems.Clock where
import PreludeExtra
import Rumpus.Systems.Controls

--import Rumpus.Systems.PlayPause

type ShouldRepeat = Bool
data TimedAction = TimedAction UTCTime DiffTime ShouldRepeat (EntityMonad ())
defineComponentKey ''TimedAction

data Clock = Clock { clkTime :: !GLfloat, clkSpeed :: !GLfloat }
defineComponentKey ''Clock

initClockSystem :: MonadState ECS m => m ()
initClockSystem = do
    registerComponent "TimedAction" myTimedAction
        (newComponentInterface myTimedAction)
    registerComponent "Clock" myClock
        (newComponentInterface myClock)

tickClockSystem :: ECSMonad ()
tickClockSystem = do
    now <- liftIO getCurrentTime

    forEntitiesWithComponent myTimedAction $ \(entityID, clockAction) -> do
        let TimedAction startTime period shouldRepeat action = clockAction
            elapsed = realToFrac $ now `diffUTCTime` startTime

        when (elapsed > period) $
            inEntity entityID $ do
                if shouldRepeat
                    then setRepeatingAction period action
                    else removeComponent myTimedAction
                action
    deltaTime <- getDeltaTime
    forEntitiesWithComponent myClock $ \(entityID, Clock currentTime clockSpeed) -> do
        let scaledDelta = deltaTime * clockSpeed
        inEntity entityID $
            myClock ==> Clock
                { clkTime = (currentTime + scaledDelta)
                , clkSpeed = clockSpeed }


setClockSpeed :: (MonadState ECS m, MonadReader EntityID m) => GLfloat -> m ()
setClockSpeed speed = do
    mClock <- getComponent myClock
    case mClock of
        Just clock -> myClock ==> clock { clkSpeed = speed }
        Nothing    -> myClock ==> Clock 0 speed

getEntityClockTime :: (MonadState ECS m) => EntityID -> m GLfloat
getEntityClockTime entityID = inEntity entityID getClockTime

getClockTime :: (MonadState ECS m, MonadReader EntityID m) => m GLfloat
getClockTime = do
    mClock <- getComponent myClock
    case mClock of
        Just clock -> return $ clkTime clock
        Nothing -> do
            setClockSpeed 1
            return 0

setTimedAction :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
               => DiffTime -> ShouldRepeat -> EntityMonad () -> m ()
setTimedAction (max 0.001 -> !period) shouldRepeat action = do
    startTime <- liftIO getCurrentTime
    myTimedAction ==> TimedAction startTime (realToFrac period) shouldRepeat action

setRepeatingAction :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
                   => DiffTime -> EntityMonad () -> m ()
setRepeatingAction !period action = setTimedAction period True action

setDelayedAction :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
                 => DiffTime -> EntityMonad () -> m ()
setDelayedAction !period action = setTimedAction period False action

