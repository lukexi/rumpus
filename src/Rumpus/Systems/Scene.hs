{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Rumpus.Systems.Scene where
import Data.ECS
import Rumpus.Types
import PreludeExtra

data Scene = Scene
    { _scnFolder  :: !FilePath
    }
makeLenses ''Scene

data SceneSystem = SceneSystem
    { _scnScene   :: !Scene
    }
makeLenses ''SceneSystem

defineSystemKey ''SceneSystem

initSceneSystem :: (MonadIO m, MonadState ECS m) => m ()
initSceneSystem = do
    let defaultScene = "Room"
    --let defaultScene = "NewObjects"
    sceneFolderName <- fromMaybe defaultScene . listToMaybe <$> liftIO getArgs


    userScenesRootDir <- getUserSceneFolder
    let userSceneFolder      = userScenesRootDir </> sceneFolderName
        pristineSceneFolder  = pristineSceneDirWithName sceneFolderName

    copyStartScene pristineSceneFolder userSceneFolder

    let
        useUserFolder = isInReleaseMode
        --useUserFolder = True
        sceneFolder = if useUserFolder
            then userSceneFolder
            else pristineSceneFolder

    registerSystem sysScene (SceneSystem (Scene sceneFolder))

startSceneSystem :: (MonadIO m, MonadState ECS m) => m ()
startSceneSystem = do
    -- Profiling doesn't support hot code load, so we can't load scenes
    -- (we use TestScene instead in Main)
    unless isBeingProfiled $ do
        loadScene =<< getSceneFolder

getSceneFolder :: MonadState ECS m => m FilePath
getSceneFolder = viewSystem sysScene (scnScene . scnFolder)

setSceneFolder :: MonadState ECS m => FilePath -> m ()
setSceneFolder sceneFolder = modifySystemState sysScene (scnScene . scnFolder .= sceneFolder)

loadScene :: (MonadIO m, MonadState ECS m) => String -> m ()
loadScene sceneFolder = do
    putStrLnIO $ "Loading scene: " ++ sceneFolder
    setSceneFolder sceneFolder

    loadEntities (sceneFolder </> ".world-state")

-- FIXME: move the .world-state concept into extensible-ecs
saveScene :: ECSMonad ()
saveScene = do
    sceneFolder <- getSceneFolder
    let stateFolder = (sceneFolder </> ".world-state")
    liftIO $ do
        removeDirectoryRecursive stateFolder
        createDirectoryIfMissing True stateFolder
    saveEntities stateFolder

fileInScene :: MonadState ECS m => FilePath -> m FilePath
fileInScene fileName = do
    sceneFolder <- getSceneFolder
    return (sceneFolder </> fileName)

-- | Copy the 'pristine' Scenes folder into the user's Documents/Rumpus directory on startup
-- if the Documents/Rumpus folder is missing.

copyStartScene :: MonadIO m => FilePath -> FilePath -> m ()
copyStartScene pristineSceneDir userSceneDir = liftIO $ do

    exists <- doesDirectoryExist userSceneDir
    when (not exists) $ do
        copyDirectory pristineSceneDir userSceneDir
            `catchIOError` (\e -> putStrLnIO ("copyStartScene: " ++ show e))

pristineSceneDirWithName :: String -> FilePath
pristineSceneDirWithName name = "pristine" </> "Scenes" </> name

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
        orM xs = or <$> sequence xs
        whenM s r = s >>= flip when r



getUserSceneFolder :: MonadIO m => m FilePath
getUserSceneFolder = liftIO $ do
    userDocsDir <- getUserDocumentsDirectory
    let userRumpusRoot   = userDocsDir </> "Rumpus"
        userSceneRoot    = userRumpusRoot </> "Scenes"
        userRedirectFile = userRumpusRoot </> "redirect.txt"

        protect f = f `catchIOError`
                        (\e -> putStrLnIO ("getUserSceneFolder: " ++ show e)
                                >> return userSceneRoot)
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
                                else return userSceneRoot
                _ -> return userSceneRoot
        else return userSceneRoot
