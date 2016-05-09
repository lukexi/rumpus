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
import Rumpus.Systems.Attachment
import Rumpus.Systems.SceneEditor
import Rumpus.Systems.CodeEditor
import Rumpus.Systems.Physics
import Rumpus.Systems.Text
import Rumpus.Systems.Scene
import Data.List (delete)
import RumpusLib
-- NOTE: this illustrates how very handy it will be to have arbitrary components;
-- rather than creating yet more maps, we can just say
-- defineComponent OpenLibrary EntityID
-- and then be able to access those instantly on the hands.
data CreatorSystem = CreatorSystem
    { _crtOpenLibrary :: !(Map WhichHand [EntityID])
    }
makeLenses ''CreatorSystem
defineSystemKey ''CreatorSystem


initCreatorSystem :: MonadState ECS m => m ()
initCreatorSystem = do
    registerSystem sysCreator (CreatorSystem mempty)



openEntityLibrary :: (MonadIO m, MonadState ECS m) => WhichHand -> m ()
openEntityLibrary whichHand = do
    sceneFolder <- getSceneFolder
    codePaths <- getDirectoryContentsWithExtension "hs" sceneFolder

    let codePathsWithNewObject = (Nothing : map Just codePaths)
        positions = goldenSectionSpiralPoints (length codePathsWithNewObject)
        positionsAndCodePaths = zip positions codePathsWithNewObject

    libraryEntities <- forM positionsAndCodePaths $ \(position, maybeCodePath) -> do
        addHandLibraryItem whichHand position maybeCodePath

    modifySystemState sysCreator $
        crtOpenLibrary . at whichHand ?= libraryEntities

addHandLibraryItem :: (MonadIO m, MonadState ECS m)
                   => WhichHand -> V3 GLfloat -> Maybe FilePath -> m EntityID
addHandLibraryItem whichHand position maybeCodePath = do
    handID   <- getHandID whichHand
    handPose <- getEntityPose handID
    newEntityID <- spawnEntity $ do
        myShape      ==> Cube
        mySize       ==> 0.01
        myProperties ==> [Floating]
        myText       ==> maybe "New Object" takeBaseName maybeCodePath
        myTextPose   ==> translateMatrix (V3 0 (-0.1) 0)
        -- Make the new object pulse
        when (isNothing maybeCodePath) $ do
            myUpdate ==> do
                now <- getNow
                setColor (colorHSL now 0.3 0.8)
        myDragBegan ==> do
            traverseM_ (getComponent myDragFrom) $ \(DragFrom handEntityID _) -> do
                entityID <- ask
                removeFromOpenLibrary whichHand entityID

                removeComponent myDragBegan
                removeComponent myText
                removeComponent myTextPose
                removeComponent myUpdate

                makeEntityPersistent entityID
                handEntityID `grabEntity` entityID
                animateSizeTo 0.3 0.3
                case maybeCodePath of
                    Just codePath -> setStartExpr codePath
                    Nothing       -> addNewStartExpr

    setEntityPose newEntityID (handPose !*! translateMatrix position)
    attachEntity handID newEntityID True

    runEntity newEntityID $ animateSizeTo 0.1 0.3
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

    modifySystemState sysCreator $
        crtOpenLibrary . at whichHand .= Nothing


addNewStartExpr :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
             => m ()
addNewStartExpr = do
    sceneFolder <- getSceneFolder
    entityID <- ask
    let defaultFilePath = "resources" </> "default-code" </> "DefaultStart" <.> "hs"
        entityFileName  = show entityID <.> "hs"
        entityFilePath  = sceneFolder </> entityFileName
    liftIO $ copyFile defaultFilePath entityFilePath

    -- Scene folder is auto-appended in CodeEditor, so we just need the filename with no path.
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
