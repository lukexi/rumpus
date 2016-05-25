module Rumpus.Systems.Scene where
import Data.ECS
import Rumpus.Types
import PreludeExtra


data SceneSystem = SceneSystem
    { _scnScene   :: !(Maybe FilePath)
    , _scnRumpusRoot :: !FilePath
    }
makeLenses ''SceneSystem

defineSystemKey ''SceneSystem

initSceneSystem :: (MonadIO m, MonadState ECS m) => m ()
initSceneSystem = do

    userRumpusRoot <- getUserRumpusRoot

    copyPristineDirIfMissing pristineDir userRumpusRoot

    let
        useUserFolder = isInReleaseMode
        --useUserFolder = True
        rootToUse = if useUserFolder
            then userRumpusRoot
            else pristineDir

    registerSystem sysScene $ SceneSystem
        { _scnScene = Nothing
        , _scnRumpusRoot = rootToUse
        }

getRumpusRootFolder :: MonadState ECS m => m FilePath
getRumpusRootFolder = viewSystem sysScene scnRumpusRoot

getSceneFolder :: MonadState ECS m => m (Maybe FilePath)
getSceneFolder = viewSystem sysScene scnScene

getSceneStateFolder :: MonadState ECS m => m (Maybe FilePath)
getSceneStateFolder = do
    maybeFolder <- getSceneFolder
    return $ stateFolderForSceneFolder <$> maybeFolder

stateFolderForSceneFolder :: FilePath -> FilePath
stateFolderForSceneFolder = (</> ".world-state")

setSceneFolder :: MonadState ECS m => FilePath -> m ()
setSceneFolder sceneFolder = modifySystemState sysScene (scnScene ?= sceneFolder)

closeScene :: (MonadIO m, MonadState ECS m) => m ()
closeScene = do
    existingEntities <- wldPersistentEntities <<.= mempty
    forM_ existingEntities removeEntity

loadScene :: (MonadIO m, MonadState ECS m) => String -> m ()
loadScene sceneFolder = do
    closeScene

    putStrLnIO $ "Loading scene: " ++ sceneFolder
    setSceneFolder sceneFolder

    let stateFolder = stateFolderForSceneFolder sceneFolder
    liftIO $ createDirectoryIfMissing True stateFolder
    loadEntities stateFolder

-- FIXME: move the .world-state concept into extensible-ecs
saveScene :: ECSMonad ()
saveScene = do
    maybeStateFolder <- getSceneStateFolder
    forM_ maybeStateFolder $ \stateFolder -> do
        liftIO $ do
            removeDirectoryRecursive stateFolder
            createDirectoryIfMissing True stateFolder
        saveEntities stateFolder

fileInScene :: MonadState ECS m => FilePath -> m (Maybe FilePath)
fileInScene fileName = do
    sceneFolder <- getSceneFolder
    return (sceneFolder <&> (</> fileName))

fileInRumpusRoot :: MonadState ECS m => FilePath -> m FilePath
fileInRumpusRoot fileName = do
    rumpusRootFolder <- getRumpusRootFolder
    return (rumpusRootFolder </> fileName)


-- | Copy the 'pristine' Scenes folder into the user's Documents/Rumpus directory on startup
-- if the Documents/Rumpus folder is missing.

copyPristineDirIfMissing :: MonadIO m => FilePath -> FilePath -> m ()
copyPristineDirIfMissing pristineSceneDir userSceneDir = liftIO $ do

    exists <- doesDirectoryExist userSceneDir
    when (not exists) $ do
        copyDirectory pristineSceneDir userSceneDir
            `catchIOError`
                (\e -> putStrLnIO ("copyPristineDirIfMissing: " ++ show e))

pristineSceneDirWithName :: String -> FilePath
pristineSceneDirWithName name = pristineDir </> name

pristineDir :: FilePath
pristineDir = "pristine"

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

versionString :: String
versionString = "0.1.2"

getUserRumpusRoot :: MonadIO m => m FilePath
getUserRumpusRoot = liftIO $ do
    userDocsDir <- getUserDocumentsDirectory
    let userRumpusRoot   = userDocsDir </> "Rumpus" </> versionString
        userRedirectFile = userRumpusRoot </> "redirect.txt"

        protect f = f `catchIOError`
                        (\e -> putStrLnIO ("getUserRumpusRoot: " ++ show e)
                                >> return userRumpusRoot)
    hasRedirect <- doesFileExist userRedirectFile
    when hasRedirect $ putStrLnIO $ "Attempting redirect"
    protect $ if hasRedirect
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
