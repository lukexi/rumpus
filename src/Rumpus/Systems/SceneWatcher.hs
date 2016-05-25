module Rumpus.Systems.SceneWatcher where
import qualified System.FSNotify as FS
import System.FSNotify hiding (Event)
import PreludeExtra
import Rumpus.Systems.Scene
import Data.HashMap.Strict as Map

data LocalFileStatus
    = Writing
    | WrittenAt UTCTime
    | Deleted
    deriving Show

-- FIXME: We only write and check the filename for events to avoid
-- dealing with converting between the absolute paths coming from FSNotify
-- and the relative paths we use during development.
-- Perhaps convert all paths to absolute?
data SceneWatcherSystem = SceneWatcherSystem
    { _swaFileStatuses   :: !(Map FilePath LocalFileStatus)
    , _swaFileEventChan  :: !(TChan FS.Event)
    , _swaStopWatcher    :: !(MVar ())
    }
makeLenses ''SceneWatcherSystem
defineSystemKey ''SceneWatcherSystem

initSceneWatcherSystem :: (MonadState ECS m, MonadIO m) => m ()
initSceneWatcherSystem = do
    fileEventChan <- liftIO newTChanIO
    stopWatcher   <- liftIO newEmptyMVar
    registerSystem sysSceneWatcher (SceneWatcherSystem mempty fileEventChan stopWatcher)

getFileStatuses :: MonadState ECS m => m (Map FilePath LocalFileStatus)
getFileStatuses = viewSystem sysSceneWatcher swaFileStatuses

getFileEventChan :: MonadState ECS m => m (TChan FS.Event)
getFileEventChan = viewSystem sysSceneWatcher swaFileEventChan

getStopWatcher :: MonadState ECS m => m (MVar ())
getStopWatcher   = viewSystem sysSceneWatcher swaStopWatcher

startSceneWatcherSystem :: (MonadState ECS m, MonadIO m) => m ()
startSceneWatcherSystem = do
    sceneStateFolder <- getSceneStateFolder
    beginWatching sceneStateFolder

beginWatching :: (MonadState ECS m, MonadIO m) => FilePath -> m ()
beginWatching folder = do
    stopWatcher <- getStopWatcher
    fileEventChan <- getFileEventChan
    _ <- liftIO . forkIO . FS.withManager $ \manager -> do
        stop <- FS.watchTree manager folder (const True) $ \e -> do
            atomically (writeTChan fileEventChan e)
        () <- takeMVar stopWatcher
        stop
    return ()

stopWatching :: (MonadState ECS m, MonadIO m) => m ()
stopWatching = do
    stopWatcher <- getStopWatcher
    liftIO $ putMVar stopWatcher ()

tickSceneWatcherSystem :: (MonadState ECS m, MonadIO m) => m ()
tickSceneWatcherSystem = do
    fileStatuses  <- getFileStatuses
    fileEventChan <- getFileEventChan
    events        <- liftIO . atomically . exhaustTChan $ fileEventChan

    forM_ events $ \event -> do
        let shouldIgnore = checkIfShouldIgnore fileStatuses event
            maybeEntityID = entityPathToEntityID (eventPath event)
        -- Ignore event if due to a local modification,
        -- or if it's not an entity file
        unless shouldIgnore $
            forM_ maybeEntityID $ \entityID ->
                case event of
                    Added    path _ -> loadEntityFile path
                    Modified path _ -> loadEntityFile path
                    Removed  _    _ -> removeEntity entityID

checkIfShouldIgnore :: Map FilePath LocalFileStatus
                    -> FS.Event -> Bool
checkIfShouldIgnore fileStatuses event =
    case Map.lookup (takeFileName $ eventPath event) fileStatuses of
        Just status ->
            checkIfShouldIgnoreDueToStatus status (eventTime event)
        Nothing -> False

checkIfShouldIgnoreDueToStatus :: LocalFileStatus -> UTCTime -> Bool
checkIfShouldIgnoreDueToStatus Writing _
    = True
checkIfShouldIgnoreDueToStatus Deleted _
    = True
checkIfShouldIgnoreDueToStatus (WrittenAt writeTime) eventtTime
    = eventtTime `diffUTCTime` writeTime < 0.01

sceneWatcherRemoveEntity :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
sceneWatcherRemoveEntity entityID = do

    sceneStateFolder <- getSceneStateFolder

    let entityPath = pathForEntity sceneStateFolder entityID
    setWatchedFileStatus entityPath Deleted

    liftIO $ removeFile entityPath
        `catchIOError` (\e ->
            putStrLn ("sceneWatcherRemoveEntity: "
                ++ show entityPath ++ " " ++ show e))
    removeEntity entityID

setWatchedFileStatus :: MonadState ECS m => FilePath -> LocalFileStatus -> m ()
setWatchedFileStatus path status =
    modifySystemState sysSceneWatcher $
        swaFileStatuses . at (takeFileName path) ?= status

sceneWatcherSaveEntity :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
sceneWatcherSaveEntity entityID = do
    sceneStateFolder <- getSceneStateFolder
    let entityPath = pathForEntity sceneStateFolder entityID

    setWatchedFileStatus entityPath Writing
    saveEntity entityID sceneStateFolder
    writtenTime <- liftIO getCurrentTime
    setWatchedFileStatus entityPath (WrittenAt writtenTime)

