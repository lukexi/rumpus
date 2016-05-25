module Rumpus.Systems.Creator where
import PreludeExtra hiding (delete)
import Rumpus.Systems.Drag
import Rumpus.Systems.Lifetime
import Rumpus.Systems.Animation
import Rumpus.Systems.Hands
import Rumpus.Systems.Shared
import Rumpus.Systems.Controls
import Rumpus.Systems.Collisions
import Rumpus.Systems.Attachment
import Rumpus.Systems.SceneEditor
import Rumpus.Systems.CodeEditor
import Rumpus.Systems.Physics
import Rumpus.Systems.Text
import Rumpus.Systems.Scene
import Rumpus.Systems.SceneWatcher
import Data.List (delete)
import RumpusLib
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

checkForDestruction :: (MonadIO m, MonadState ECS m) => WhichHand -> m ()
checkForDestruction whichHand = do
    let otherHand = getOtherHand whichHand
    maybePendingDestruction <- viewSystem sysCreator (crtPendingDestruction . at otherHand)
    forM_ maybePendingDestruction $ \destroyID -> do
        sceneWatcherRemoveEntity destroyID

openEntityLibrary :: (MonadIO m, MonadState ECS m) => WhichHand -> m ()
openEntityLibrary whichHand = do
    rumpusRoot <- getRumpusRootFolder
    codePaths  <- getDirectoryContentsWithExtension "hs" rumpusRoot

    let codePathsWithNewObject = (Nothing : map Just codePaths)
        positions = goldenSectionSpiralPoints (length codePathsWithNewObject)
        positionsAndCodePaths = zip positions codePathsWithNewObject

    libraryEntities <- forM positionsAndCodePaths $ \(position, maybeCodePath) -> do
        addHandLibraryItem whichHand position maybeCodePath

    destructionOrb <- addDestructionOrb whichHand

    exitOrb <- addExitOrb whichHand

    modifySystemState sysCreator $
        crtOpenLibrary . at whichHand ?= destructionOrb:exitOrb:libraryEntities

addExitOrb :: (MonadIO m, MonadState ECS m)
           => WhichHand -> m EntityID
addExitOrb whichHand = do

    let otherHand = getOtherHand whichHand
    otherHandID <- getOtherHandID whichHand

    let normalPulse = do
            now <- getNow
            let brightness = (* 0.5) . (+1) . (/2) . sin $ now
            setColor (colorHSL 0.4 0.8 brightness)
    exitOrbID <- spawnEntity $ do
        myColor      ==> (colorHSL 0.7 0.8 0)
        myShape      ==> Sphere
        mySize       ==> 0.01
        myProperties ==> [Floating]
        myUpdate     ==> normalPulse
        myDragBegan ==> do
            closeEntityLibrary whichHand
            closeScene
        -- Pulse the other hand when it hovers over us
        myColliding ==> \entityID _ -> do
            when (entityID == otherHandID) $ do
                hapticPulse otherHand 1000

    handID   <- getHandID whichHand
    handPose <- getEntityPose handID
    setEntityPose exitOrbID (handPose !*! translateMatrix exitOrbOffset)
    attachEntityToEntity handID exitOrbID False

    inEntity exitOrbID $ animateSizeTo 0.05 0.3
    return exitOrbID

addDestructionOrb :: (MonadIO m, MonadState ECS m)
                   => WhichHand -> m EntityID
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
    handPose <- getEntityPose handID
    destructorID <- spawnEntity $ do
        myColor      ==> (colorHSL 0.7 0.8 0)
        myShape      ==> Sphere
        mySize       ==> 0.01
        myProperties ==> [Floating]
        myUpdate     ==> normalPulse

        -- When an entity collides with us that's held by the other hand,
        -- grow and change color to indicate that the object will be deleted on release.
        -- Set the object as "pending destruction".
        myCollisionStart ==> \entityID _ -> do
            otherHandID <- getOtherHandID whichHand
            isBeingHeldByOtherHand <- isEntityAttachedTo entityID otherHandID
            when isBeingHeldByOtherHand $ do
                animateSizeTo 0.6 0.3
                myUpdate ==> angryPulse
                modifySystemState sysCreator $
                    crtPendingDestruction . at whichHand ?= entityID
        -- Pulse the hand holding the item while hovering over the orb
        myColliding ==> \_entityID _ -> do
            let otherHand = getOtherHand whichHand
            hapticPulse otherHand 1100
        -- When the collision ends, either due to the object being dropped or
        -- because the user changed their mind and pulled away, return to normal
        -- size and clear the object from "pending destruction"
        myCollisionEnd ==> \entityID -> do
            pendingDestruction <- viewSystem sysCreator (crtPendingDestruction . at whichHand)
            when (pendingDestruction == Just entityID) $ do
                myUpdate ==> normalPulse
                animateSizeTo 0.05 0.3
                modifySystemState sysCreator $
                    crtPendingDestruction . at whichHand .= Nothing


    setEntityPose destructorID (handPose !*! translateMatrix creatorOffset)
    attachEntityToEntity handID destructorID False

    inEntity destructorID $ animateSizeTo 0.05 0.3
    return destructorID

creatorOffset :: V3 GLfloat
creatorOffset = V3 0 0 -0.4

exitOrbOffset :: V3 GLfloat
exitOrbOffset = V3 0 0 0.4

getOtherHand :: WhichHand -> WhichHand
getOtherHand whichHand = case whichHand of
    LeftHand  -> RightHand
    RightHand -> LeftHand
getOtherHandID :: MonadState ECS m => WhichHand -> m EntityID
getOtherHandID whichHand = getHandID (getOtherHand whichHand)

-- FIXME: it would be extra cool to add the startExpr immediately rather than on grab,
-- so that we can see tiny versions of the code on each object.
-- This would require a 'paused' flag to keep the script from actually running,
-- and we would not want code on the NewObject (or, just make sure it isn't copied right away.)
-- See if this is a performance problem.
addHandLibraryItem :: (MonadIO m, MonadState ECS m)
                   => WhichHand -> V3 GLfloat -> Maybe FilePath -> m EntityID
addHandLibraryItem whichHand spherePosition maybeCodePath = do
    handID   <- getHandID whichHand
    handPose <- getEntityPose handID
    newEntityID <- spawnEntity $ do
        myShape      ==> Cube
        mySize       ==> V3 0.01 0.01 0.001
        myProperties ==> [Floating]
        myText       ==> maybe "New Object" takeBaseName maybeCodePath
        myTextPose   ==> mkTransformation
                            (axisAngle (V3 1 0 0) (0))
                            (V3 0 (-1) 0)
                            !*! scaleMatrix 0.3
        myColor      ==> V4 0.1 0.1 0.1 1
        -- Make the new object pulse
        when (isNothing maybeCodePath) $ do
            myUpdate ==> do
                now <- getNow
                setColor (colorHSL now 0.3 0.8)
        myDragBegan ==> do
            traverseM_ (getComponent myDragFrom) $ \(DragFrom handEntityID _) -> do
                entityID <- ask
                removeFromOpenLibrary whichHand entityID

                removeComponent myUpdate
                removeComponent myDragBegan
                removeComponent myText
                removeComponent myTextPose
                removeTextRendererComponent

                makeEntityPersistent entityID
                handEntityID `grabEntity` entityID
                animateSizeTo 0.3 0.3
                case maybeCodePath of
                    Just codePath -> setStartExpr codePath
                    Nothing       -> addNewStartExpr

    -- Hand is usually held vertically, so we rotate objects such that they'll
    -- have their code facing towards the user in that case
    setEntityPose newEntityID
        (handPose !*! translateMatrix creatorOffset
                  !*! mkTransformation (axisAngle (V3 1 0 0) (-pi/2)) (spherePosition * 0.2))
    attachEntityToEntity handID newEntityID False

    inEntity newEntityID $ animateSizeTo 0.05 0.3
    return newEntityID

removeFromOpenLibrary :: MonadState ECS m => WhichHand -> EntityID -> m ()
removeFromOpenLibrary whichHand entityID =
    modifySystemState sysCreator $
        crtOpenLibrary . ix whichHand %= delete entityID

closeEntityLibrary :: (MonadIO m, MonadState ECS m) => WhichHand -> m ()
closeEntityLibrary whichHand = do
    libraryEntities <- fromMaybe [] <$> viewSystem sysCreator (crtOpenLibrary . at whichHand)
    forM_ libraryEntities $ \entityID ->
        inEntity entityID $ do
            -- remove the size animation to avoid
            -- animation system/lifetime end animation conflicts
            removeComponent mySizeAnimation
            setLifetime 0.3

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
    rumpusRoot <- getRumpusRootFolder
    files <- getDirectoryContentsWithExtension "hs" rumpusRoot

    let newObjectCodeName = findNextNumberedName "MyObject" (map takeBaseName files)
        entityFileName    = newObjectCodeName <.> "hs"
        entityFilePath    = rumpusRoot </> entityFileName
    liftIO $ writeFile entityFilePath (defaultStartCodeWithModuleName newObjectCodeName)

    -- Rumpus folder is auto-appended in CodeEditor, so we just need the filename with no path.
    setStartExpr entityFileName


setStartExpr :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
             => FilePath -> m ()
setStartExpr fileName = do
    let codeInFile = (fileName, "start")
    myStartExpr ==> codeInFile
    registerWithCodeEditor codeInFile myStart



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
