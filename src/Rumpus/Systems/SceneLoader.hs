module Rumpus.Systems.SceneLoader where
import PreludeExtra hiding (delete, catch)
import Rumpus.Systems.Drag
import Rumpus.Systems.Lifetime
import Rumpus.Systems.Animation
import Rumpus.Systems.Hands
import Rumpus.Systems.Shared
import Rumpus.Systems.Controls
import Rumpus.Systems.Collisions
import Rumpus.Systems.Attachment
import Rumpus.Systems.SceneEditor
import Rumpus.Systems.Scene
import Rumpus.Systems.CodeEditor
import Rumpus.Systems.PlayPause
import Rumpus.Systems.Physics
import Rumpus.Systems.Text
import Rumpus.Systems.Scene
import Rumpus.Systems.SceneWatcher
import Rumpus.Types
import Data.List (delete, isPrefixOf)
import Control.Exception

import RumpusLib


{-

FIXME:
need
[x] call initSceneLoaderSystem
[x] need startSceneLoaderSystem that calls showSceneLoader on startup
    (and later, save last loaded scene and restore it)
[ ] disable creator when in sceneloader
[ ] Fix Scene.hs trying to load old scenes/userscenes no matter what
[ ] Fix SceneWatcher to only add and delete objects in the current scene folder
    (use takeDirectory . makeRelative $ rumpusRoot)
-}

data SceneLoaderSystem = SceneLoaderSystem
    { _sclSceneIcons        :: ![EntityID]
    }
makeLenses ''SceneLoaderSystem
defineSystemKey ''SceneLoaderSystem


initSceneLoaderSystem :: MonadState ECS m => m ()
initSceneLoaderSystem = do
    registerSystem sysSceneLoader (SceneLoaderSystem mempty)


startSceneLoaderSystem :: (MonadIO m, MonadState ECS m) => m ()
startSceneLoaderSystem = do
    -- Profiling doesn't support hot code load, so we can't load scenes
    -- (we use TestScene instead in Main)
    unless isBeingProfiled $ do
        showSceneLoader

listScenes :: (MonadState ECS m, MonadIO m) => m [FilePath]
listScenes = do
    rumpusRoot <- getRumpusRootFolder

    scenePaths <- listDirectories rumpusRoot
    return scenePaths

showSceneLoader :: (MonadState ECS m, MonadIO m) => m ()
showSceneLoader = do
    setPlayerPosition 0

    scenePaths <- listScenes
    let scenePathsWithNewScene = Nothing : map Just scenePaths
        positions = goldenSectionSpiralPoints (length scenePathsWithNewScene)
        positionsAndCodePaths = zip positions scenePathsWithNewScene

    libraryEntities <- forM positionsAndCodePaths $ \(position, maybeCodePath) -> do
        addSceneLibraryItem position maybeCodePath

    modifySystemState sysSceneLoader $
        sclSceneIcons .= libraryEntities

hideSceneLoader = do
    iconIDs <- viewSystem sysSceneLoader sclSceneIcons
    forM_ iconIDs removeEntity

sceneLoaderAnimationInitialSize = V3 0.01 0.01 0.01
sceneLoaderAnimationFinalSize   = V3 0.3  0.3  0.3
sceneLoaderAnimationDuration    = 0.3

listDirectories inPath = liftIO $
    filterM (doesDirectoryExist . (inPath </>)) =<< getDirectoryContentsSafe inPath

addSceneLibraryItem :: (MonadIO m, MonadState ECS m)
                    => V3 GLfloat -> Maybe FilePath -> m EntityID
addSceneLibraryItem spherePosition maybeScenePath = do
    newEntityID <- spawnEntity $ do
        myPose       ==> translateMatrix (spherePosition * 1 + V3 0 1 0)
        myShape      ==> Cube
        mySize       ==> sceneLoaderAnimationInitialSize
        myProperties ==> [Floating]
        myText       ==> maybe "New Scene" takeBaseName maybeScenePath
        myTextPose   ==> mkTransformation
                            (axisAngle (V3 1 0 0) (0))
                            (V3 0 (-1) 0)
                            !*! scaleMatrix 0.3
        myColor      ==> V4 0.1 0.1 0.1 1
        -- Make the new object pulse
        when (isNothing maybeScenePath) $ do
            myUpdate ==> do
                now <- getNow
                setColor (colorHSL now 0.3 0.8)
        myDragBegan ==> do
            rumpusRoot <- getRumpusRootFolder
            mScenePathToLoad <- case maybeScenePath of
                Just scenePath -> return (Just (rumpusRoot </> scenePath))
                Nothing -> do
                    -- FIXME two users could create a new scene at once and we don't handle this
                    scenePaths <- listScenes
                    let newSceneName = findNextNumberedName "MyScene" scenePaths
                        newScenePath = rumpusRoot </> newSceneName

                    -- Do nothing if we can't create the folder
                    createdSuccessfully <- createDirectorySafe newScenePath
                    if createdSuccessfully
                        then return (Just newScenePath)
                        else return Nothing
            forM_ mScenePathToLoad $ \scenePathToLoad -> do
                setWorldPlaying False
                hideSceneLoader
                loadScene scenePathToLoad

    inEntity newEntityID $
        animateSizeTo sceneLoaderAnimationFinalSize 0.3
    return newEntityID

createDirectorySafe dirName = liftIO (do
    createDirectoryIfMissing True dirName
    return True
    `catch` (\e -> do
        putStrLn ("Error in getDirectoryContentsSafe: " ++ show (e :: IOException))
        return False))
