module Home where
import Rumpus

libraryCenter :: V3 GLfloat
libraryCenter = V3 0 1.5 0

minimizedItemSize :: V3 GLfloat
minimizedItemSize = 0.01

itemSize :: V3 GLfloat
itemSize = 0.3

sceneTransitionTime :: Float
sceneTransitionTime = 0.1

start :: Start
start = do
    setPlayerPosition 0

    sceneNames <- listScenes
    let sceneNamesWithNewScene = Nothing : map Just sceneNames
        numItems               = length sceneNamesWithNewScene
        positions              = goldenSectionSpiralPoints numItems
        positionsAndCodePaths  = zip3 [0..] positions sceneNamesWithNewScene

    forM_ positionsAndCodePaths $ \(n, pos, maybeCodePath) -> do
        addSceneLibraryItem (fromIntegral n / fromIntegral numItems) pos maybeCodePath

    return ()

addSceneLibraryItem :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
                    => GLfloat -> V3 GLfloat -> Maybe FilePath -> m ()
addSceneLibraryItem n spherePosition maybeSceneName = do
    let itemPosition = spherePosition * 1 + libraryCenter
    spawnEntity_ $ do
        myPose         ==> position itemPosition
        myShape        ==> Sphere
        mySize         ==> minimizedItemSize
        myBody         ==> Animated
        myDragOverride ==> True
        myText         ==> fromMaybe "New Scene" maybeSceneName
        myTextPose     ==> positionRotationScale
                            (V3 0 (-1) 0)
                            (axisAngle (V3 0 1 0) pi)
                            0.3
        myColor        ==> colorHSL n 0.5 0.5
        myStart        ==> animateSizeTo itemSize 0.3
        myUpdate ==> do
            -- Make the new object pulse
            when (isNothing maybeSceneName) $ do
                now <- getNow
                setColor (colorHSL now 0.3 0.8)

            -- Orient items towards head
            headPose <- getHeadPose
            setPose $ orientToward itemPosition (headPose ^. translation) (V3 0 1 0)
        myDragBegan ==> do
            sceneName <- case maybeSceneName of
                Just sceneName -> return sceneName
                Nothing        -> createNewScene

            -- Fade out while loading
            fadeToColor (V4 1 1 1 1) sceneTransitionTime

            -- Load after 1 second
            setDelayedAction sceneTransitionTime $ do
                fadeToColor (V4 0 0 0 0) sceneTransitionTime
                loadScene sceneName


