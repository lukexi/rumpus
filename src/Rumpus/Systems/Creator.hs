{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Rumpus.Systems.Creator where
import PreludeExtra
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
import Rumpus.Systems.Scene

data CreatorSystem = CreatorSystem
    { _crtPrimedEntities :: !(Map WhichHand EntityID)
    }
makeLenses ''CreatorSystem
defineSystemKey ''CreatorSystem

setPrimedEntity whichHand newEntityID =
    modifySystemState sysCreator $
        crtPrimedEntities . at whichHand ?= newEntityID

unsetPrimedEntity whichHand =
    modifySystemState sysCreator $
        crtPrimedEntities . at whichHand .= Nothing


initCreatorSystem :: MonadState ECS m => m ()
initCreatorSystem = do
    registerSystem sysCreator (CreatorSystem mempty)

unprimeNewEntity :: (MonadIO m, MonadState ECS m) => WhichHand -> m ()
unprimeNewEntity whichHand = do
    mEntityID <- viewSystem sysCreator (crtPrimedEntities . at whichHand)
    forM_ mEntityID $ \entityID ->
        runEntity entityID (setLifetime 0.3)
    unsetPrimedEntity whichHand

primeNewEntity :: (MonadIO m, MonadState ECS m) => WhichHand -> m ()
primeNewEntity whichHand = do

    newEntityID <- spawnEntity $ do
        myShape     ==> Cube
        mySize      ==> 0.01
        myUpdate    ==> do
            now <- getNow
            setColor (colorHSL now 0.3 0.8)

        myDragBegan ==> do
            traverseM_ (getComponent myDragFrom) $ \(DragFrom handEntityID _) -> do
                unsetPrimedEntity whichHand
                removeComponent myDragBegan
                entityID <- ask
                handEntityID `grabEntity` entityID
                animateSizeTo 0.3 0.3
                addCodeExpr "Start" "start" myStartExpr myStart

    handID   <- getHandID whichHand
    handPose <- getEntityPose handID
    setEntityPose (handPose !*! translateMatrix (V3 0 0 (-0.25))) newEntityID
    attachEntity handID newEntityID True

    runEntity newEntityID $ animateSizeTo 0.1 0.3

    setPrimedEntity whichHand newEntityID


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
