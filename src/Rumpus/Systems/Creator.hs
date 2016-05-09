{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

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
import Data.List (delete, isPrefixOf)
import RumpusLib
-- NOTE: this illustrates how very handy it will be to have arbitrary components;
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

checkForDestruction whichHand = do
    let otherHand = getOtherHand whichHand
    maybePendingDestruction <- viewSystem sysCreator (crtPendingDestruction . at otherHand)
    forM_ maybePendingDestruction $ \destroyID -> do
        removeEntity destroyID

openEntityLibrary :: (MonadIO m, MonadState ECS m) => WhichHand -> m ()
openEntityLibrary whichHand = do
    sceneFolder <- getSceneFolder
    codePaths <- getDirectoryContentsWithExtension "hs" sceneFolder

    let codePathsWithNewObject = (Nothing : map Just codePaths)
        positions = goldenSectionSpiralPoints (length codePathsWithNewObject)
        positionsAndCodePaths = zip positions codePathsWithNewObject

    libraryEntities <- forM positionsAndCodePaths $ \(position, maybeCodePath) -> do
        addHandLibraryItem whichHand position maybeCodePath

    destructionOrb <- addDestructionOrb whichHand

    modifySystemState sysCreator $
        crtOpenLibrary . at whichHand ?= destructionOrb:libraryEntities

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
    newEntityID <- spawnEntity $ do
        myShape      ==> Sphere
        mySize       ==> 0.01
        myProperties ==> [Floating]
        myUpdate     ==> normalPulse

        myCollisionEnd ==> \entityID -> do
            pendingDestruction <- viewSystem sysCreator (crtPendingDestruction . at whichHand)
            when (pendingDestruction == Just entityID) $ do
                myUpdate ==> normalPulse
                animateSizeTo 0.05 0.3
                modifySystemState sysCreator $
                    crtPendingDestruction . at whichHand .= Nothing
        myCollisionStart ==> \entityID _ -> do
            otherHandID <- getOtherHandID whichHand
            isBeingHeldByOtherHand <- isEntityAttachedTo entityID otherHandID
            when isBeingHeldByOtherHand $ do
                animateSizeTo 0.6 0.3
                myUpdate ==> angryPulse
                modifySystemState sysCreator $
                    crtPendingDestruction . at whichHand ?= entityID

    setEntityPose newEntityID (handPose !*! translateMatrix creatorOffset)
    attachEntity handID newEntityID False

    runEntity newEntityID $ animateSizeTo 0.05 0.3
    return newEntityID

creatorOffset = V3 0 0 (-0.4)

getOtherHand whichHand = case whichHand of
    LeftHand  -> RightHand
    RightHand -> LeftHand
getOtherHandID whichHand = getHandID (getOtherHand whichHand)

addHandLibraryItem :: (MonadIO m, MonadState ECS m)
                   => WhichHand -> V3 GLfloat -> Maybe FilePath -> m EntityID
addHandLibraryItem whichHand spherePosition maybeCodePath = do
    handID   <- getHandID whichHand
    handPose <- getEntityPose handID
    newEntityID <- spawnEntity $ do
        myShape      ==> Cube
        mySize       ==> 0.01
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
    attachEntity handID newEntityID False

    runEntity newEntityID $ animateSizeTo 0.05 0.3
    return newEntityID

removeFromOpenLibrary :: MonadState ECS m => WhichHand -> EntityID -> m ()
removeFromOpenLibrary whichHand entityID =
    modifySystemState sysCreator $
        crtOpenLibrary . ix whichHand %= delete entityID

closeEntityLibrary :: (MonadIO m, MonadState ECS m) => WhichHand -> m ()
closeEntityLibrary whichHand = do
    libraryEntities <- fromMaybe [] <$> viewSystem sysCreator (crtOpenLibrary . at whichHand)
    forM_ libraryEntities $ \entityID ->
        runEntity entityID (setLifetime 0.3)

    modifySystemState sysCreator $ do
        crtPendingDestruction . at whichHand .= Nothing
        crtOpenLibrary . at whichHand .= Nothing


addNewStartExpr :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
             => m ()
addNewStartExpr = do
    sceneFolder <- getSceneFolder
    files <- getDirectoryContentsWithExtension "hs" sceneFolder

    let newObjectCodeName = findNextNumberedName "MyObject" (map takeBaseName files)
    entityID <- ask
    let defaultFilePath = "resources" </> "default-code" </> "DefaultStart" <.> "hs"
        entityFileName  = newObjectCodeName <.> "hs"
        entityFilePath  = sceneFolder </> entityFileName
    liftIO $ copyFile defaultFilePath entityFilePath

    -- Scene folder is auto-appended in CodeEditor, so we just need the filename with no path.
    setStartExpr entityFileName

-- | Given a list of names like [NewObject1, NewObject3, NewObject7]
findNextNumberedName name inList =
    let newObjects = filter (isPrefixOf name) inList
        existingNumbers = catMaybes $ map (readMaybe . drop (length name)) newObjects :: [Int]
    in name ++ show (succ (maximum existingNumbers))

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
    sceneFolder <- getSceneFolder
    entityID <- ask
    let defaultFilePath = "resources" </> "default-code" </> "Default" ++ fileName <.> "hs"
        entityFileName = (show entityID ++ "-" ++ fileName) <.> "hs"
        entityFilePath = sceneFolder </> entityFileName
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
        runEntity toEntityID $ do
            withComponent_ codeFileComponentKey unregisterWithCodeEditor
            codeFileComponentKey ==> newCodeInFile
            registerWithCodeEditor newCodeInFile codeFileComponentKey
-}
--
