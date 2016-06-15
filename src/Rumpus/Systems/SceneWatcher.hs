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

-- NOTE: these are all absolute paths - we could use the `path` package to ensure that.
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
    rumpusRootFolder <- getRumpusRootFolder
    beginWatching rumpusRootFolder

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

    sceneStateFolder <- getSceneStateFolderAbsolute

    -- Watch for events in the current scene
    -- FIXME: we don't live-watch for new scenes, so
    -- users won't see them if they're created while
    -- they're in the SceneLoader (until they re-enter it)
    -- FIXME: if we remove the file-watchers in text-gl and subhalive,
    -- we'll need to watch the library files outside the scene
    -- (in RumpusRoot) to actuate code/text editor update

    forM_ events $ \event -> do
        -- Ignore event if
        --     | outside scene state folder,
        --     | due to a local modification,
        --     | or if it's not an entity file

        let shouldIgnore = checkIfShouldIgnore sceneStateFolder fileStatuses event
        unless shouldIgnore $ do
            let maybeEntityID = entityPathToEntityID (eventPath event)
            forM_ maybeEntityID $ \entityID ->
                case event of
                    Added    path _ -> loadEntityFile path
                    Modified path _ -> loadEntityFile path
                    Removed  _    _ -> removeEntity entityID

checkIfShouldIgnore :: FilePath
                    -> Map FilePath LocalFileStatus
                    -> FS.Event
                    -> Bool
checkIfShouldIgnore sceneStateFolder fileStatuses event
    | takeDirectory (eventPath event) /= sceneStateFolder = True
    | otherwise =
        case Map.lookup (eventPath event) fileStatuses of
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

    entityPath <- liftIO . makeAbsolute $ pathForEntity sceneStateFolder entityID
    setWatchedFileStatus entityPath Deleted

    liftIO $ removeFile entityPath
        `catchIOError` (\e ->
            putStrLn ("sceneWatcherRemoveEntity: "
                ++ show entityPath ++ " " ++ show e))
    removeEntity entityID

setWatchedFileStatus :: MonadState ECS m => FilePath -> LocalFileStatus -> m ()
setWatchedFileStatus path status =
    modifySystemState sysSceneWatcher $
        swaFileStatuses . at path ?= status

sceneWatcherSaveEntity :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
sceneWatcherSaveEntity entityID = do
    sceneStateFolder <- getSceneStateFolder
    putStrLnIO $ "Saving to " ++ sceneStateFolder
    entityPath <- liftIO . makeAbsolute $ pathForEntity sceneStateFolder entityID

    setWatchedFileStatus entityPath Writing
    saveEntity entityID sceneStateFolder
    writtenTime <- liftIO getCurrentTime
    setWatchedFileStatus entityPath (WrittenAt writtenTime)

