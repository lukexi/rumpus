module Rumpus.Systems.Scene where
import Data.ECS
import Rumpus.Types
import Rumpus.Systems.PlayPause
import PreludeExtra hiding (catch)
import RumpusLib
import Control.Exception

data SceneSystem = SceneSystem
    { _scnSceneName  :: !String
    , _scnRumpusRoot :: !FilePath
    }
makeLenses ''SceneSystem

defineSystemKey ''SceneSystem

pristineDir :: FilePath
pristineDir = "pristine"

initSceneSystem :: (MonadIO m, MonadState ECS m) => m ()
initSceneSystem = do

    userRumpusRoot <- determineUserRumpusRoot

    copyPristineDirIfMissing pristineDir userRumpusRoot
    copyRedirectFileIfMissing

    let rootToUse = if useUserFolder
            then userRumpusRoot
            else pristineDir
        useUserFolder = isInReleaseMode
        --useUserFolder = True
    putStrLnIO ("Using rumpus root: " ++ rootToUse)

    registerSystem sysScene $ SceneSystem
        { _scnSceneName  = "Lobby"
        , _scnRumpusRoot = rootToUse
        }

getRumpusRootFolder :: MonadState ECS m => m FilePath
getRumpusRootFolder = viewSystem sysScene scnRumpusRoot

getSceneName :: MonadState ECS m => m String
getSceneName = viewSystem sysScene scnSceneName


getSceneFolder :: MonadState ECS m => m FilePath
getSceneFolder =
    sceneFolderForScene =<< getSceneName

getSceneStateFolder :: MonadState ECS m => m FilePath
getSceneStateFolder =
    stateFolderForSceneFolder <$> getSceneFolder

getSceneStateFolderAbsolute :: (MonadIO m, MonadState ECS m) => m FilePath
getSceneStateFolderAbsolute =
    liftIO . makeAbsolute =<< getSceneStateFolder

listDirectories :: MonadIO m => FilePath -> m [FilePath]
listDirectories inPath = liftIO $
    filterM (doesDirectoryExist . (inPath </>)) =<< getDirectoryContentsSafe inPath

listScenes :: (MonadState ECS m, MonadIO m) => m [String]
listScenes = do
    rumpusRoot <- getRumpusRootFolder
    sceneNames <- sort <$> listDirectories rumpusRoot
    return sceneNames


stateFolderForSceneFolder :: FilePath -> FilePath
stateFolderForSceneFolder = (</> ".world-state")

setSceneName :: MonadState ECS m => FilePath -> m ()
setSceneName sceneFolder = modifySystemState sysScene (scnSceneName .= sceneFolder)

closeScene :: (MonadBaseControl IO m, MonadIO m, MonadState ECS m) => m ()
closeScene = do
    existingEntities <- wldPersistentEntities <<.= mempty
    forM_ existingEntities removeEntity

loadScene :: (MonadBaseControl IO m, MonadIO m, MonadState ECS m) => String -> m ()
loadScene sceneName = do

    setWorldPlaying False

    rumpusRoot <- getRumpusRootFolder
    let sceneFolder = rumpusRoot </> sceneName

    closeScene

    putStrLnIO $ "Loading scene: " ++ sceneFolder
    setSceneName sceneName

    let stateFolder = stateFolderForSceneFolder sceneFolder
    liftIO $ createDirectoryIfMissing True stateFolder
    loadEntities stateFolder

doesSceneExist :: (MonadIO m, MonadState ECS m) => FilePath -> m Bool
doesSceneExist sceneName = do
    sceneFolder <- sceneFolderForScene sceneName
    liftIO (doesDirectoryExist sceneFolder)

sceneFolderForScene :: MonadState ECS m => FilePath -> m FilePath
sceneFolderForScene sceneName = do
    rumpusRoot <- getRumpusRootFolder
    return (rumpusRoot </> sceneName)

fileInCurrentScene :: MonadState ECS m => FilePath -> m FilePath
fileInCurrentScene fileName = do
    sceneFolder <- getSceneFolder
    return (sceneFolder </> fileName)

fileInRumpusRoot :: MonadState ECS m => FilePath -> m FilePath
fileInRumpusRoot fileName = do
    rumpusRootFolder <- getRumpusRootFolder
    return (rumpusRootFolder </> fileName)

createNewScene :: (MonadIO m, MonadState ECS m) => m (Maybe String)
createNewScene = createNewSceneNamed "MyScene"

createNewSceneNamed :: (MonadIO m, MonadState ECS m) => String -> m (Maybe String)
createNewSceneNamed baseName = do
    rumpusRoot <- getRumpusRootFolder
    -- FIXME two users could create a new scene at once and we don't handle this
    scenePaths <- listScenes
    let newSceneName = findNextNumberedName baseName scenePaths
        newScenePath = rumpusRoot </> newSceneName

    -- Do nothing if we can't create the folder
    createdSuccessfully <- createDirectorySafe newScenePath
    if createdSuccessfully
        then return (Just newSceneName)
        else return Nothing



-- | Copy the 'pristine' Scenes folder into the user's Documents/Rumpus directory on startup
-- if the Documents/Rumpus folder is missing.

copyPristineDirIfMissing :: MonadIO m => FilePath -> FilePath -> m ()
copyPristineDirIfMissing pristineSceneDir userSceneDir = liftIO $ do

    exists <- doesDirectoryExist userSceneDir
    when (not exists) $ do
        copyDirectory pristineSceneDir userSceneDir
            `catchIOError`
                (\e -> putStrLnIO ("copyPristineDirIfMissing: " ++ show e))

copyRedirectFileIfMissing :: MonadIO m => m ()
copyRedirectFileIfMissing = liftIO $ do

    userDocsDir <- getUserDocumentsDirectory
    -- We look for the redirect file in the root of the rumpus directory
    let userRumpusRoot   = userDocsDir </> "Rumpus"

    exists <- doesFileExist (userRumpusRoot </> "redirect.txt")
    when (not exists) $ do
        copyFile ("resources" </> "redirect.txt") (userRumpusRoot </> "redirect.txt")
            `catchIOError`
                (\e -> putStrLnIO ("copyRedirectFileIfMissing: " ++ show e))



determineUserRumpusRoot :: MonadIO m => m FilePath
determineUserRumpusRoot = liftIO $ do
    userDocsDir <- getUserDocumentsDirectory
    -- We look for the redirect file in the root of the rumpus directory
    let userRumpusRoot   = userDocsDir </> "Rumpus"
        userRedirectFile = userDocsDir </> "Rumpus" </> "redirect.txt"

        protect f = f `catchIOError`
                        (\e -> putStrLnIO ("determineUserRumpusRoot: " ++ show e)
                                >> return userRumpusRoot)
    hasRedirect <- doesFileExist userRedirectFile
    if hasRedirect
        then putStrLnIO $ "Found redirect"
        else putStrLnIO $ "No redirect found in " ++ userRedirectFile
    rumpusRoot <- protect $ if hasRedirect
        then do
            redirectContents <- readFile userRedirectFile
            let maybeLine = fmap (dropWhile isSpace) . listToMaybe . lines
                            $ redirectContents
            case maybeLine of
                Just pathLine
                    | isAbsolute pathLine -> do
                            redirectPath <- canonicalizePath pathLine
                            exists <- doesDirectoryExist redirectPath
                            if exists
                                then return redirectPath
                                else return userRumpusRoot
                _ -> return userRumpusRoot
        else return userRumpusRoot

    -- Append the version string
    return (rumpusRoot </> versionString)




copyDirectory :: MonadIO m => FilePath -> FilePath -> m ()
copyDirectory src dst = liftIO $ do
    whenM (not <$> doesDirectoryExist src) $
        throwM (userError "source does not exist")
    whenM (doesFileOrDirectoryExist dst) $
        throwM (userError "destination already exists")

    createDirectoryIfMissing True dst
    contents <- filter (`notElem` [".", ".."]) <$> getDirectoryContents src
    forM_ contents $ \name -> do
        let srcPath = src </> name
        let dstPath = dst </> name
        isDirectory <- doesDirectoryExist srcPath
        if isDirectory
            then copyDirectory srcPath dstPath
            else copyFile      srcPath dstPath

    where
        doesFileOrDirectoryExist x = orM [doesDirectoryExist x, doesFileExist x]
        orM :: Monad m => [m Bool] -> m Bool
        orM xs = or <$> sequence xs
        whenM s r = s >>= flip when r

createDirectorySafe :: MonadIO m => FilePath -> m Bool
createDirectorySafe dirName = liftIO (do
    createDirectoryIfMissing True dirName
    return True
    `catch` (\e -> do
        putStrLn ("Error in getDirectoryContentsSafe: " ++ show (e :: IOException))
        return False))
