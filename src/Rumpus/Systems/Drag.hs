module Rumpus.Systems.Drag where
import PreludeExtra

import Rumpus.Systems.Hands
import Rumpus.Systems.Shared
import Rumpus.Systems.CodeProtect

data DragFrom = DragFrom HandEntityID (M44 GLfloat)

type Drag = M44 GLfloat -> EntityMonad ()
type DragBegan = EntityMonad ()
type DragEnded = EntityMonad ()
type DragOverride = Bool
defineComponentKey ''DragFrom
defineComponentKey ''DragBegan
defineComponentKey ''DragEnded
defineComponentKey ''Drag
defineComponentKey ''DragOverride --

getEntityDragOverride :: (MonadState ECS m) => EntityID -> m Bool
getEntityDragOverride entityID = fromMaybe False <$> getEntityComponent entityID myDragOverride

initDragSystem :: MonadState ECS m => m ()
initDragSystem = do
    registerComponent "DragFrom"     myDragFrom     (newComponentInterface myDragFrom)
    registerComponent "DragBegan"    myDragBegan    (newComponentInterface myDragBegan)
    registerComponent "DragEnded"    myDragEnded    (newComponentInterface myDragEnded)
    registerComponent "Drag"         myDrag         (newComponentInterface myDrag)
    registerComponent "DragOverride" myDragOverride (newComponentInterface myDragOverride)

beginDrag :: EntityID -> EntityID -> ECSMonad ()
beginDrag handEntityID draggedID = do
    startM44 <- getEntityPose handEntityID
    setEntityComponent myDragFrom (DragFrom handEntityID startM44) draggedID

    inEntity draggedID $
        withComponent_ myDragBegan $ \dragBegan ->
            runUserFunctionProtected myDragBegan dragBegan

continueDrag :: HandEntityID -> ECSMonad ()
continueDrag draggingHandEntityID = do
    forEntitiesWithComponent myDragFrom $ \(entityID, DragFrom handEntityID startM44) ->
        when (handEntityID == draggingHandEntityID) $ do
            currentM44 <- getEntityPose handEntityID
            let dragDistance = currentM44 `subtractMatrix` startM44

            inEntity entityID $
                withComponent_ myDrag $ \drag ->
                    runUserFunctionProtected myDrag (drag dragDistance)

endDrag :: HandEntityID -> ECSMonad ()
endDrag endingDragHandEntityID = do
    forEntitiesWithComponent myDragFrom $ \(entityID, DragFrom handEntityID _) -> do
        when (handEntityID == endingDragHandEntityID) $ do
            removeEntityComponent myDragFrom entityID

            inEntity entityID $
                withComponent_ myDragEnded $ \dragEnded ->
                    runUserFunctionProtected myDragEnded dragEnded
