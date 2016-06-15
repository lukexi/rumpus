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
import Control.Exception

import RumpusLib
libraryCenter :: V3 GLfloat
libraryCenter = V3 0 1.5 0

start :: Start
start = do
    setPlayerPosition 0

    scenePaths <- listScenes
    let scenePathsWithNewScene = Nothing : map Just scenePaths
        numItems               = length scenePathsWithNewScene
        positions              = goldenSectionSpiralPoints numItems
        positionsAndCodePaths  = zip3 [0..] positions scenePathsWithNewScene

    forM_ positionsAndCodePaths $ \(n, pos, maybeCodePath) -> do
        addSceneLibraryItem (fromIntegral n / fromIntegral numItems) pos maybeCodePath
    _ <- spawnChildInstance "Stars"
    --_ <- spawnChildInstance "VoicePillars"
    _ <- spawnChildInstance "Platform"
    _ <- spawnChildInstance "Grass"

    return ()

sceneLoaderAnimationInitialSize :: V3 GLfloat
sceneLoaderAnimationInitialSize = V3 0.01 0.01 0.01

sceneLoaderAnimationFinalSize :: V3 GLfloat
sceneLoaderAnimationFinalSize   = V3 0.3  0.3  0.3

sceneLoaderAnimationDuration :: Double
sceneLoaderAnimationDuration    = 0.3

addSceneLibraryItem :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
                    => GLfloat -> V3 GLfloat -> Maybe FilePath -> m ()
addSceneLibraryItem n spherePosition maybeScenePath = do
    let itemPosition = spherePosition * 1 + libraryCenter
    spawnEntity_ $ do
        myPose         ==> position itemPosition
        myShape        ==> Sphere
        mySize         ==> sceneLoaderAnimationInitialSize
        myBody         ==> Animated
        myDragOverride ==> True
        myText         ==> maybe "New Scene" takeBaseName maybeScenePath
        myTextPose     ==> positionRotationScale
                            (V3 0 (-1) 0)
                            (axisAngle (V3 0 1 0) pi)
                            0.3
        myColor        ==> colorHSL n 0.5 0.5
        -- Make the new object pulse
        myUpdate ==> do
            when (isNothing maybeScenePath) $ do
                now <- getNow
                setColor (colorHSL now 0.3 0.8)

            -- Orient items towards head
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
                    loadScene scenePathToLoad
        myStart ==> animateSizeTo sceneLoaderAnimationFinalSize 0.3

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

