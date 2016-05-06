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

data CreatorSystem = CreatorSystem
    { _crtNewEntityID :: !(Map WhichHand EntityID)
    }
makeLenses ''CreatorSystem
defineSystemKey ''CreatorSystem

initCreatorSystem :: MonadState ECS m => m ()
initCreatorSystem = do
    registerSystem sysCreator (CreatorSystem mempty)

unprimeNewEntity :: (MonadIO m, MonadState ECS m) => WhichHand -> m ()
unprimeNewEntity whichHand = do
    mEntityID <- viewSystem sysCreator (crtNewEntityID . at whichHand)
    forM_ mEntityID $ \entityID ->
        runEntity entityID (setLifetime 0.3)

primeNewEntity :: (MonadIO m, MonadState ECS m) => WhichHand -> m ()
primeNewEntity whichHand = do

    newEntityID <- spawnEntity $ do
        myShape     ==> Cube
        mySize      ==> 0.01
        myUpdate    ==> do
            now <- getNow
            setColor (colorHSL now 0.3 0.8)

         --It might be more elegant to use the exclusive-attachment system
         --and just piggyback on SceneEditor's grabbing functionality here.
        myDragBegan ==> do
            traverseM_ (getComponent myDragFrom) $ \(DragFrom handEntityID _) -> do
                removePendingEntity whichHand
                removeComponent myDragBegan
                entityID <- ask
                handEntityID `grabEntity` entityID
                --addCodeExpr "Start" "start" myStartExpr myStart

    handID   <- getHandID whichHand
    handPose <- getEntityPose handID
    setEntityPose (handPose !*! translateMatrix (V3 0 0 (-0.25))) newEntityID
    attachEntity handID newEntityID True

    runEntity newEntityID $ animateSizeTo 0.1 0.3

    setPendingEntity whichHand newEntityID

setPendingEntity whichHand newEntityID =
    modifySystemState sysCreator $
        crtNewEntityID . at whichHand ?= newEntityID
removePendingEntity whichHand =
    modifySystemState sysCreator $
        crtNewEntityID . at whichHand .= Nothing
