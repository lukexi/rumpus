module Rumpus.Systems.Creator where
import PreludeExtra hiding (delete)
import Rumpus.Systems.Drag
import Rumpus.Systems.Animation
import Rumpus.Systems.Hands
import Rumpus.Systems.Shared
import Rumpus.Systems.Clock
--import Rumpus.Systems.CodeEditor
import Rumpus.Systems.Controls
import Rumpus.Systems.Collisions
import Rumpus.Systems.Attachment
import Rumpus.Systems.CodeEditor
import Rumpus.Systems.KeyPads
import Rumpus.Systems.Physics
import Rumpus.Systems.Synth
import Rumpus.Systems.Text
import Rumpus.Systems.Scene
import Rumpus.Systems.SceneWatcher
import Data.List (delete)
import RumpusLib

activeDestructorOrbSize :: V3 GLfloat
activeDestructorOrbSize = 0.6

destructorOrbSize :: V3 GLfloat
destructorOrbSize = 0.05

exitOrbSize :: V3 GLfloat
exitOrbSize = 0.05

initialLibraryItemSize :: V3 GLfloat
initialLibraryItemSize = 0.001

libraryItemSize :: V3 GLfloat
libraryItemSize = V3 0.05 0.05 0.01

newEntitySize :: V3 GLfloat
newEntitySize = V3 0.4 0.4 0.1

animDur :: DiffTime
animDur = 0.3

creatorOffset :: V3 GLfloat
creatorOffset = V3 0 0 -0.4

exitOrbOffset :: V3 GLfloat
exitOrbOffset = V3 0 0 0.2

-- NOTE: this illustrates how handy it will be to have arbitrary components;
-- rather than creating yet more maps, we can just say
-- defineComponent OpenLibrary EntityID
-- and then be able to access those instantly on the hands.
data CreatorSystem = CreatorSystem
    { _crtLibraryItems       :: !(Map WhichHand [EntityID])
    , _crtPendingDestruction :: !(Map WhichHand EntityID)
    }
makeLenses ''CreatorSystem
defineSystemKey ''CreatorSystem


initCreatorSystem :: MonadState ECS m => m ()
initCreatorSystem = do
    registerSystem sysCreator (CreatorSystem mempty mempty)

-- See if an object is currently touching the destruction orb, and destroy it if so.
-- This is called when the hands release their grip on an item.
checkForDestruction :: (MonadIO m, MonadState ECS m) => WhichHand -> m Bool
checkForDestruction whichHand = do
    let otherHand = otherHandFrom whichHand
    maybePendingDestruction <- viewSystem sysCreator (crtPendingDestruction . at otherHand)
    forM_ maybePendingDestruction $ \destroyID -> do
        sceneWatcherRemoveEntity destroyID
    return (isJust maybePendingDestruction)

openEntityLibrary whichHand = do
    sceneName <- getSceneName
    openEntityLibraryForScene whichHand sceneName

openEntityLibraryForScene :: (MonadIO m, MonadState ECS m) => WhichHand -> String -> m ()
openEntityLibraryForScene whichHand sceneName = do
    modifySystemState sysCreator $ crtLibraryItems . at whichHand ?= []

    sceneFolder <- sceneFolderForScene sceneName
    codePaths   <- sort <$> getDirectoryContentsWithExtension "hs" sceneFolder

    currentSceneName <- getSceneName
    let isCurrentScene = sceneName == currentSceneName
        buttons = if isCurrentScene then [ToScenesItem "Import", NewObjectItem] else [ToScenesItem "Back"]
        codePathsWithNewObject = buttons ++ map (ObjectItem sceneName) codePaths
        positions              = goldenSectionSpiralPoints (length codePathsWithNewObject)
        positionsAndCodePaths  = zip positions codePathsWithNewObject

    forM_ positionsAndCodePaths $ \(pos, item) -> do
        addObjectLibraryItem whichHand 0 pos item

    addDestructionOrb whichHand

    addExitOrb whichHand


data ItemType = ObjectItem String FilePath
              | NewObjectItem
              | SceneItem String
              | ToScenesItem String -- Text is "Import" or "Back" depending on if we're viewing the current scene

openSceneLibrary :: (MonadIO m, MonadState ECS m) => WhichHand -> m ()
openSceneLibrary whichHand = do
    modifySystemState sysCreator $ crtLibraryItems . at whichHand ?= []

    sceneNames <- listScenes

    let numItems = length sceneNames
        positions = goldenSectionSpiralPoints numItems
        positionsAndSceneNames = zip3 [0..] positions sceneNames

    forM_ positionsAndSceneNames $ \(n, pos, sceneName) -> do
        addObjectLibraryItem whichHand (fromIntegral n / fromIntegral numItems) pos (SceneItem sceneName)

    addDestructionOrb whichHand

    addExitOrb whichHand

addEntityToOpenLibrary :: (MonadState ECS m) => WhichHand -> EntityID -> m ()
addEntityToOpenLibrary whichHand entityID = modifySystemState sysCreator $ crtLibraryItems . ix whichHand %= (entityID:)


-- FIXME: it would be extra cool to add the startExpr immediately rather than on grab,
-- so that we can see tiny versions of the code on each object.
-- This would require a 'paused' flag to keep the script from actually running,
-- and we would not want code on the NewObject (or, just make sure it isn't copied right away.)
-- See if this is a performance problem.
addObjectLibraryItem :: (MonadIO m, MonadState ECS m)
                     => WhichHand -> GLfloat -> V3 GLfloat -> ItemType -> m ()
addObjectLibraryItem whichHand n spherePosition itemType = do
    newEntityID <- case itemType of
        ObjectItem sceneName codeName -> makeEntityItem whichHand spherePosition (Just (sceneName, codeName))
        NewObjectItem                 -> makeEntityItem whichHand spherePosition Nothing
        SceneItem sceneName           -> makeSceneItem whichHand n spherePosition sceneName
        ToScenesItem title            -> makeToScenesItem whichHand spherePosition title

    -- Hand is usually held vertically, so we rotate objects such that they'll
    -- have their code facing towards the user in that case
    handID   <- getHandID whichHand
    attachEntityToEntity handID newEntityID
        (positionRotation
            (creatorOffset + spherePosition * 0.2)
            (axisAngle (V3 1 0 0) (-pi/2)))

    inEntity newEntityID $ animateSizeTo libraryItemSize animDur
    addEntityToOpenLibrary whichHand newEntityID

makeSceneItem whichHand hue spherePosition sceneName = spawnEntity $ do
    myShape      ==> Sphere
    mySize       ==> initialLibraryItemSize
    myBody       ==> Animated
    myText       ==> sceneName
    myTextPose   ==> position (V3 0 (-1) 0) !*! scaleMatrix 0.3
    myColor      ==> colorHSL hue 0.5 0.5
    myDragBegan  ==> do
        closeCreator whichHand
        openEntityLibraryForScene whichHand sceneName

makeToScenesItem whichHand spherePosition title = spawnEntity $ do
    myShape      ==> Sphere
    mySize       ==> initialLibraryItemSize
    myBody       ==> Animated
    myText       ==> title
    myTextPose   ==> position (V3 0 (-1) 0) !*! scaleMatrix 0.3
    myUpdate     ==> do
        now <- getNow
        setColor (colorHSL now 0.5 0.5)
    myDragBegan  ==> do
        closeCreator whichHand
        openSceneLibrary whichHand


makeEntityItem whichHand spherePosition maybeCodePath = spawnEntity $ do
    myShape      ==> Cube
    mySize       ==> initialLibraryItemSize
    myBody       ==> Animated
    myText       ==> maybe "New Object" (takeBaseName . snd) maybeCodePath
    myTextPose   ==> position (V3 0 (-1) 0) !*! scaleMatrix 0.3
    myColor      ==> V4 0.1 0.1 0.1 1
    -- Make the new object pulse
    when (isNothing maybeCodePath) $ do
        myUpdate ==> do
            now <- getNow
            setColor (colorHSL now 0.3 0.8)
    myDragBegan ==> do
        -- Remove this instance from the library so it is not deleted when the library is closed
        entityID <- ask
        removeFromOpenLibrary whichHand entityID

        -- Re-add a new instance to the library so the user can spawn us again
        addObjectLibraryItem whichHand 0 spherePosition (maybe NewObjectItem (uncurry ObjectItem) maybeCodePath)

        -- Don't select right away; wait for a second click.
        -- This is cleaner when "using" objects (e.g. note, seed)
        -- where you don't always care about the code
        clearSelection


        removeComponent myUpdate
        removeComponent myDragBegan
        removeComponent myText
        removeComponent myTextPose
        removeTextRendererComponent

        makeEntityPersistent entityID

        animateSizeTo newEntitySize animDur
        case maybeCodePath of
            Just (sourceSceneName, codePath) -> do
                currentSceneName <- getSceneName

                when (sourceSceneName /= currentSceneName) $ do
                    sourceFolder <- sceneFolderForScene sourceSceneName
                    targetFolder <- sceneFolderForScene currentSceneName
                    liftIO $ copyFile (sourceFolder </> codePath) (targetFolder </> codePath)

                setStartExpr (codePath, "start")
            Nothing       -> addNewStartExpr

removeFromOpenLibrary :: MonadState ECS m => WhichHand -> EntityID -> m ()
removeFromOpenLibrary whichHand entityID =
    modifySystemState sysCreator $
        crtLibraryItems . ix whichHand %= delete entityID

closeCreator :: (MonadIO m, MonadState ECS m) => WhichHand -> m ()
closeCreator whichHand = do
    libraryEntities <- fromMaybe [] <$> viewSystem sysCreator (crtLibraryItems . at whichHand)
    forM_ libraryEntities $ \entityID ->
        inEntity entityID $ do
            removeComponent myDragBegan -- Don't allow drag during close animation
            animateSizeTo initialLibraryItemSize animDur
            setDelayedAction animDur (removeEntity =<< ask)

    modifySystemState sysCreator $ do
        crtPendingDestruction . at whichHand .= Nothing
        crtLibraryItems . at whichHand .= Nothing














defaultStartCodeWithModuleName :: String -> String
defaultStartCodeWithModuleName moduleName = unlines
    [ "module " ++ moduleName ++ " where"
    , "import Rumpus"
    , ""
    , "start :: Start"
    , "start = do"
    , "    return ()"
    ]

addNewStartExpr :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
                => m ()
addNewStartExpr = do
    codeInFile <- createNewStartExpr

    -- Rumpus folder is auto-appended in CodeEditor, so we just need the filename with no path.
    setStartExpr codeInFile

createNewStartExpr :: (MonadIO m, MonadState ECS m) => m CodeInFile
createNewStartExpr = do
    rumpusRoot <- getRumpusRootFolder
    files <- getDirectoryContentsWithExtension "hs" rumpusRoot

    let newObjectName = findNextNumberedName "MyObject" (map takeBaseName files)
    createStartExpr newObjectName

createStartExpr :: (MonadIO m, MonadState ECS m) => String -> m CodeInFile
createStartExpr name = do
    rumpusRoot <- getRumpusRootFolder
    let entityFileName    = name <.> "hs"
        entityFilePath    = rumpusRoot </> entityFileName
    liftIO $ writeFile entityFilePath (defaultStartCodeWithModuleName name)
    return (entityFileName, "start")

addExitOrb :: (MonadIO m, MonadState ECS m)
           => WhichHand -> m ()
addExitOrb whichHand = do

    let otherHand = otherHandFrom whichHand
    otherHandID <- getHandID otherHand

    let normalPulse = do
            now <- getNow
            let brightness = (* 0.5) . (+1) . (/2) . sin $ now
            setColor (colorHSL 0.4 0.8 brightness)
    exitOrbID <- spawnEntity $ do
        myColor        ==> colorHSL 0.4 0.8 0.5
        myShape        ==> Sphere
        mySize         ==> initialLibraryItemSize
        myBody         ==> Detector
        myDragOverride ==> True
        myUpdate       ==> normalPulse
        -- Pulse the other hand when it hovers over us
        myCollisionContinues ==> \entityID _ -> do
            when (entityID == otherHandID) $ do
                hapticPulse otherHand 1000
        myDragBegan ==> do
            fadeToColor 1 1
            -- Add the delayed action to the hand, since the exit orb will disappear
            -- when the user releases the library button
            inEntity otherHandID $ setDelayedAction 1 $ do
                fadeToColor 0 1
                closeCreator whichHand
                releasePolyPatches
                closeScene
                loadScene "Home"

    handID   <- getHandID whichHand
    attachEntityToEntity handID exitOrbID (translateMatrix exitOrbOffset)

    inEntity exitOrbID $ animateSizeTo exitOrbSize animDur

    addEntityToOpenLibrary whichHand exitOrbID

addDestructionOrb :: (MonadIO m, MonadState ECS m)
                   => WhichHand -> m ()
addDestructionOrb whichHand = do
    let normalPulse = do
            now <- getNow
            let brightness = (* 0.5) . (+1) . (/2) . sin $ now
            setColor (colorHSL 0.7 0.8 brightness)
        angryPulse = do
            now <- getNow
            let brightness = (* 0.5) . (+1) . (/2) . sin . (*10) $ now
            setColor (colorHSL 0.1 0.8 brightness)
    handID   <- getHandID whichHand
    destructorID <- spawnEntity $ do
        myColor      ==> colorHSL 0.7 0.8 0
        myShape      ==> Sphere
        mySize       ==> initialLibraryItemSize
        myBody       ==> Detector
        myBodyFlags  ==> [Ungrabbable]
        myUpdate     ==> normalPulse

        -- When an entity collides with us that's held by the other hand,
        -- grow and change color to indicate that the object will be deleted on release.
        -- Set the object as "pending destruction".
        myCollisionBegan ==> \entityID _ -> do
            isBeingHeldByOtherHand <- isEntityBeingHeldByHand entityID (otherHandFrom whichHand)
            when isBeingHeldByOtherHand $ do
                animateSizeTo activeDestructorOrbSize animDur
                myUpdate ==> angryPulse
                modifySystemState sysCreator $
                    crtPendingDestruction . at whichHand ?= entityID
        -- Pulse the hand holding the item while hovering over the orb
        myCollisionContinues ==> \_entityID _ -> do
            let otherHand = otherHandFrom whichHand
            hapticPulse otherHand 1100
        -- When the collision ends, either due to the object being dropped or
        -- because the user changed their mind and pulled away, return to normal
        -- size and clear the object from "pending destruction"
        myCollisionEnded ==> \entityID -> do
            pendingDestruction <- viewSystem sysCreator (crtPendingDestruction . at whichHand)
            when (pendingDestruction == Just entityID) $ do
                myUpdate ==> normalPulse
                animateSizeTo destructorOrbSize animDur
                modifySystemState sysCreator $
                    crtPendingDestruction . at whichHand .= Nothing


    attachEntityToEntity handID destructorID (translateMatrix creatorOffset)

    inEntity destructorID $ animateSizeTo destructorOrbSize animDur
    addEntityToOpenLibrary whichHand destructorID


-----------------------------------------------
-- Experiments in dynamic code addition/cloning
-----------------------------------------------


{-
forkCode :: (MonadIO m, MonadState ECS m) => EntityID -> EntityID -> m ()
forkCode fromEntityID toEntityID = do
    let codeFileComponentKey = myStartExpr
    mCodeExpr <- getEntityComponent fromEntityID codeFileComponentKey

    forM_ mCodeExpr $ \(fullPath, expr) -> do
        let (path, fileName) = splitFileName fullPath
            (name, ext)      = splitExtension fileName

        existing <- getDirectoryContentsWithExtension "hs" rumpusRoot
        let newObjectName = findNextNumberedName name (map takeBaseName existing)

        let newFullPath = path </> newObjectName <.> ext
        liftIO $ copyFile fullPath newFullPath

        let newCodeInFile = (newFullPath, expr)
        inEntity toEntityID $ do
            withComponent_ codeFileComponentKey unregisterWithCodeEditor
            codeFileComponentKey ==> newCodeInFile
            registerWithCodeEditor newCodeInFile codeFileComponentKey
-}
--
