module Rumpus.Systems.SceneLoader where
import PreludeExtra hiding (delete, catch)
import Rumpus.Systems.Drag
import Rumpus.Systems.Animation
import Rumpus.Systems.Clock
import Rumpus.Systems.Shared
import Rumpus.Systems.Controls
import Rumpus.Systems.Scene
import Rumpus.Systems.PlayPause
import Rumpus.Systems.Physics
import Rumpus.Systems.Text
import Rumpus.Systems.CodeEditor
--import Rumpus.Systems.Scene
--import Rumpus.Systems.SceneWatcher
--import Rumpus.Types
import Control.Exception
import Data.List (sort)

import RumpusLib
--import Rumpus.Types
libraryCenter :: V3 GLfloat
libraryCenter = V3 0 1.5 0
--libraryCenter = if isInReleaseMode
    --then V3 0 1.5 0
    -- offset for couch access
    --else V3 -1.5 1.5 0

data SceneLoaderSystem = SceneLoaderSystem
    { _sclSceneLoaderRootID :: !(Maybe EntityID)
    }
makeLenses ''SceneLoaderSystem
defineSystemKey ''SceneLoaderSystem


initSceneLoaderSystem :: MonadState ECS m => m ()
initSceneLoaderSystem = do
    registerSystem sysSceneLoader (SceneLoaderSystem Nothing)


startSceneLoaderSystem :: (MonadIO m, MonadState ECS m) => m ()
startSceneLoaderSystem = do
    -- Profiling doesn't support hot code load, so we can't load scenes
    -- (we use TestScene instead in Main)
    --unless isBeingProfiled $ do

    listToMaybe <$> liftIO getArgs >>= \case
        Nothing -> showSceneLoader
        Just sceneName -> do
            rumpusRoot <- getRumpusRootFolder
            loadScene (rumpusRoot </> sceneName)

listScenes :: (MonadState ECS m, MonadIO m) => m [FilePath]
listScenes = do
    rumpusRoot <- getRumpusRootFolder

    scenePaths <- sort <$> listDirectories rumpusRoot
    return scenePaths

showSceneLoader :: (MonadState ECS m, MonadIO m) => m ()
showSceneLoader = do
    setPlayerPosition 0

    sceneLoaderRootID <- spawnEntity (return ())
    modifySystemState sysSceneLoader $
        sclSceneLoaderRootID ?= sceneLoaderRootID


    scenePaths <- listScenes
    let scenePathsWithNewScene = Nothing : map Just scenePaths
        positions = goldenSectionSpiralPoints (length scenePathsWithNewScene)
        positionsAndCodePaths = zip positions scenePathsWithNewScene

    inEntity sceneLoaderRootID $ do
        forM_ positionsAndCodePaths $ \(pos, maybeCodePath) -> do
            addSceneLibraryItem pos maybeCodePath
        _ <- spawnChildInstance "Stars"
        _ <- spawnChildInstance "VoicePillars"
        _ <- spawnChildInstance "Platform"
        _ <- spawnChildInstance "Grass"
        return ()
    return ()

hideSceneLoader :: (MonadState ECS m, MonadIO m) => m ()
hideSceneLoader = do
    modifySystemState sysSceneLoader $ do
        mRootID <- use sclSceneLoaderRootID
        lift $ forM_ mRootID removeEntity
        sclSceneLoaderRootID .= Nothing

sceneLoaderAnimationInitialSize :: V3 GLfloat
sceneLoaderAnimationInitialSize = V3 0.01 0.01 0.01

sceneLoaderAnimationFinalSize :: V3 GLfloat
sceneLoaderAnimationFinalSize   = V3 0.3  0.3  0.3

sceneLoaderAnimationDuration :: Double
sceneLoaderAnimationDuration    = 0.3

listDirectories :: MonadIO m => FilePath -> m [FilePath]
listDirectories inPath = liftIO $
    filterM (doesDirectoryExist . (inPath </>)) =<< getDirectoryContentsSafe inPath

addSceneLibraryItem :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
                    => V3 GLfloat -> Maybe FilePath -> m ()
addSceneLibraryItem spherePosition maybeScenePath = do
    let itemPosition = spherePosition * 1 + libraryCenter
    itemID <- spawnChild $ do
        myPose         ==> position itemPosition
        myShape        ==> Sphere
        mySize         ==> sceneLoaderAnimationInitialSize
        myBody         ==> Animated
        myDragOverride ==> True
        myText         ==> maybe "New Scene" takeBaseName maybeScenePath
        myTextPose     ==> positionRotation
                            (V3 0 (-1) 0)
                            (axisAngle (V3 0 1 0) pi)
                            !*! scaleMatrix 0.3
        myColor      ==> V4 0.1 0.1 0.1 1
        -- Make the new object pulse
        myUpdate ==> do
            when (isNothing maybeScenePath) $ do
                now <- getNow
                setColor (colorHSL now 0.3 0.8)
            headPose <- getHeadPose
            setPose $ orientToward itemPosition (headPose ^. translation) (V3 0 1 0)
        myDragBegan ==> do
            rumpusRoot <- getRumpusRootFolder
            mScenePathToLoad <- case maybeScenePath of
                Just scenePath -> return (Just (rumpusRoot </> scenePath))
                Nothing        -> createNewScene
            forM_ mScenePathToLoad $ \scenePathToLoad -> do
                fadeToColor (V4 1 1 1 1) 1
                setDelayedAction 1 $ do
                    fadeToColor (V4 0 0 0 0) 1
                    setWorldPlaying False
                    hideSceneLoader
                    loadScene scenePathToLoad

    inEntity itemID $
        animateSizeTo sceneLoaderAnimationFinalSize 0.3

createNewScene :: (MonadIO m, MonadState ECS m) => m (Maybe FilePath)
createNewScene = do
    rumpusRoot <- getRumpusRootFolder
    -- FIXME two users could create a new scene at once and we don't handle this
    scenePaths <- listScenes
    let newSceneName = findNextNumberedName "MyScene" scenePaths
        newScenePath = rumpusRoot </> newSceneName

    -- Do nothing if we can't create the folder
    createdSuccessfully <- createDirectorySafe newScenePath
    if createdSuccessfully
        then return (Just newScenePath)
        else return Nothing

createDirectorySafe :: MonadIO m => FilePath -> m Bool
createDirectorySafe dirName = liftIO (do
    createDirectoryIfMissing True dirName
    return True
    `catch` (\e -> do
        putStrLn ("Error in getDirectoryContentsSafe: " ++ show (e :: IOException))
        return False))
