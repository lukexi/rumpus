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
import Rumpus.Systems.SceneLoader
import Data.List (delete, sort)
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
    { _crtOpenLibrary        :: !(Map WhichHand [EntityID])
    , _crtPendingDestruction :: !(Map WhichHand EntityID)
    }
makeLenses ''CreatorSystem
defineSystemKey ''CreatorSystem


initCreatorSystem :: MonadState ECS m => m ()
initCreatorSystem = do
    registerSystem sysCreator (CreatorSystem mempty mempty)

checkForDestruction :: (MonadIO m, MonadState ECS m) => WhichHand -> m Bool
checkForDestruction whichHand = do
    let otherHand = otherHandFrom whichHand
    maybePendingDestruction <- viewSystem sysCreator (crtPendingDestruction . at otherHand)
    forM_ maybePendingDestruction $ \destroyID -> do
        sceneWatcherRemoveEntity destroyID
    return (isJust maybePendingDestruction)

openEntityLibrary :: (MonadIO m, MonadState ECS m) => WhichHand -> m ()
openEntityLibrary whichHand = do
    aSceneIsOpen <- isJust <$> getSceneFolder
    when aSceneIsOpen $ do
        modifySystemState sysCreator $ crtOpenLibrary . at whichHand ?= []

        rumpusRoot <- getRumpusRootFolder
        codePaths  <- sort <$> getDirectoryContentsWithExtension "hs" rumpusRoot

        let codePathsWithNewObject = (Nothing : map Just codePaths)
            positions = goldenSectionSpiralPoints (length codePathsWithNewObject)
            positionsAndCodePaths = zip positions codePathsWithNewObject

        forM_ positionsAndCodePaths $ \(pos, maybeCodePath) -> do
            addHandLibraryItem whichHand pos maybeCodePath

        addDestructionOrb whichHand

        addExitOrb whichHand

addEntityToOpenLibrary :: (MonadState ECS m) => WhichHand -> EntityID -> m ()
addEntityToOpenLibrary whichHand entityID = modifySystemState sysCreator $ crtOpenLibrary . ix whichHand %= (entityID:)

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
        myColor      ==> (colorHSL 0.7 0.8 0)
        myShape      ==> Sphere
        mySize       ==> initialLibraryItemSize
        myBody       ==> Detector -- FIXME: We can't make this Ungrabbable as that inhibits clicks
        myUpdate     ==> normalPulse
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
                closeEntityLibrary whichHand
                releasePolyPatches
                closeScene
                showSceneLoader

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
        myColor      ==> (colorHSL 0.7 0.8 0)
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


-- FIXME: it would be extra cool to add the startExpr immediately rather than on grab,
-- so that we can see tiny versions of the code on each object.
-- This would require a 'paused' flag to keep the script from actually running,
-- and we would not want code on the NewObject (or, just make sure it isn't copied right away.)
-- See if this is a performance problem.
addHandLibraryItem :: (MonadIO m, MonadState ECS m)
                   => WhichHand -> V3 GLfloat -> Maybe FilePath -> m ()
addHandLibraryItem whichHand spherePosition maybeCodePath = do
    handID   <- getHandID whichHand
    newEntityID <- spawnEntity $ do
        myShape      ==> Cube
        mySize       ==> initialLibraryItemSize
        myBody       ==> Animated
        myText       ==> maybe "New Object" takeBaseName maybeCodePath
        myTextPose   ==> positionRotation
                            (V3 0 (-1) 0)
                            (axisAngle (V3 1 0 0) (0))
                            !*! scaleMatrix 0.3
        myColor      ==> V4 0.1 0.1 0.1 1
        -- Make the new object pulse
        when (isNothing maybeCodePath) $ do
            myUpdate ==> do
                now <- getNow
                setColor (colorHSL now 0.3 0.8)
        myDragBegan ==> do

            -- Re-add ourselves to the library so the user can spawn us again
            addHandLibraryItem whichHand spherePosition maybeCodePath

            -- Don't select right away; wait for a second click.
            -- This is cleaner when "using" objects (e.g. note, seed)
            -- where you don't always care about the code
            clearSelection

            entityID <- ask
            removeFromOpenLibrary whichHand entityID

            removeComponent myUpdate
            removeComponent myDragBegan
            removeComponent myText
            removeComponent myTextPose
            removeTextRendererComponent

            makeEntityPersistent entityID

            animateSizeTo newEntitySize animDur
            case maybeCodePath of
                Just codePath -> setStartExpr (codePath, "start")
                Nothing       -> addNewStartExpr

    -- Hand is usually held vertically, so we rotate objects such that they'll
    -- have their code facing towards the user in that case
    attachEntityToEntity handID newEntityID
        (positionRotation
            (creatorOffset + spherePosition * 0.2)
            (axisAngle (V3 1 0 0) (-pi/2)))

    inEntity newEntityID $ animateSizeTo libraryItemSize animDur
    addEntityToOpenLibrary whichHand newEntityID

removeFromOpenLibrary :: MonadState ECS m => WhichHand -> EntityID -> m ()
removeFromOpenLibrary whichHand entityID =
    modifySystemState sysCreator $
        crtOpenLibrary . ix whichHand %= delete entityID

closeEntityLibrary :: (MonadIO m, MonadState ECS m) => WhichHand -> m ()
closeEntityLibrary whichHand = do
    libraryEntities <- fromMaybe [] <$> viewSystem sysCreator (crtOpenLibrary . at whichHand)
    forM_ libraryEntities $ \entityID ->
        inEntity entityID $ do
            removeComponent myDragBegan -- Don't allow drag during close animation
            animateSizeTo initialLibraryItemSize animDur
            setDelayedAction animDur (removeEntity =<< ask)

    modifySystemState sysCreator $ do
        crtPendingDestruction . at whichHand .= Nothing
        crtOpenLibrary . at whichHand .= Nothing

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



{-
addCodeExpr :: (MonadIO m, MonadState ECS m, MonadReader EntityID m, Typeable a)
            => FilePath
            -> String
            -> Key (EntityMap CodeInFile)
            -> Key (EntityMap a)
            -> m ()
addCodeExpr fileName exprName codeFileComponentKey codeComponentKey = do
    rumpusRoot <- getRumpusRootFolder
    entityID <- ask
    let defaultFilePath = "resources" </> "default-code" </> "Default" ++ fileName <.> "hs"
        entityFileName = (show entityID ++ "-" ++ fileName) <.> "hs"
        entityFilePath = rumpusRoot </> entityFileName
        codeFile = (entityFileName, exprName)
    liftIO $ copyFile defaultFilePath entityFilePath
    codeFileComponentKey ==> codeFile
    registerWithCodeEditor codeFile codeComponentKey
-}


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
        -- Slightly tricky to get right without overwriting files; need to enumerate directory and find unused name
        -- so using getPosixTime for now.
        --fileNum          = succ . fromMaybe 1 . readMaybe . reverse . takeWhile isDigit . reverse $ fileName
        now <- liftIO $ getPOSIXTime
        let newName = name ++ show now
            newFullPath = path </> newName <.> ext
        liftIO $ copyFile fullPath newFullPath

        let newCodeInFile = (newFullPath, expr)
        inEntity toEntityID $ do
            withComponent_ codeFileComponentKey unregisterWithCodeEditor
            codeFileComponentKey ==> newCodeInFile
            registerWithCodeEditor newCodeInFile codeFileComponentKey
-}
--
