{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Systems.Drag where
import PreludeExtra

import Rumpus.Systems.Hands
import Rumpus.Systems.Shared

-- data DragFrom = DragFrom HandEntityID (M44 GLfloat)
data DragFrom = DragFrom HandEntityID (V3 GLfloat)

type Drag = V3 GLfloat -> EntityMonad ()
type DragBegan = EntityMonad ()

defineComponentKey ''DragFrom
defineComponentKey ''DragBegan
defineComponentKey ''Drag


initDragSystem :: MonadState ECS m => m ()
initDragSystem = do

    registerComponent "DragFrom"  myDragFrom  (newComponentInterface myDragFrom)
    registerComponent "DragBegan" myDragBegan (newComponentInterface myDragBegan)
    registerComponent "Drag"      myDrag      (newComponentInterface myDrag)

beginDrag :: EntityID -> EntityID -> ECSMonad ()
beginDrag handEntityID draggedID = do
    startPos <- view translation <$> getEntityPose handEntityID
    setEntityComponent myDragFrom (DragFrom handEntityID startPos) draggedID

    runEntity draggedID $
        withComponent_ myDragBegan id

continueDrag :: HandEntityID -> ECSMonad ()
continueDrag draggingHandEntityID = do
    forEntitiesWithComponent myDragFrom $ \(entityID, DragFrom handEntityID startPos) ->
        when (handEntityID == draggingHandEntityID) $ do
            currentPose <- view translation <$> getEntityPose handEntityID
            let dragDistance = currentPose - startPos

            runEntity entityID $
                withComponent_ myDrag ($ dragDistance)

endDrag :: MonadState ECS m => HandEntityID -> m ()
endDrag endingDragHandEntityID = do
    forEntitiesWithComponent myDragFrom $ \(entityID, DragFrom handEntityID _) -> do
        when (handEntityID == endingDragHandEntityID) $ do
            removeEntityComponent myDragFrom entityID
