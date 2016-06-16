module Rumpus.Systems.Drag where
import PreludeExtra

import Rumpus.Systems.Hands
import Rumpus.Systems.Shared
import Rumpus.Systems.CodeEditor

data ActiveDrag = ActiveDrag HandEntityID (M44 GLfloat)

type DragBegan     = EntityMonad ()
type DragEnded     = EntityMonad ()
type DragContinues = M44 GLfloat -> EntityMonad ()

type DragOverride = Bool
defineComponentKey ''ActiveDrag
defineComponentKey ''DragBegan
defineComponentKey ''DragEnded
defineComponentKey ''DragContinues
defineComponentKey ''DragOverride

getEntityDragOverride :: (MonadState ECS m) => EntityID -> m Bool
getEntityDragOverride entityID = fromMaybe False <$> getEntityComponent entityID myDragOverride

initDragSystem :: MonadState ECS m => m ()
initDragSystem = do
    registerComponent "ActiveDrag"    myActiveDrag    (newComponentInterface myActiveDrag)
    registerComponent "DragBegan"     myDragBegan     (newComponentInterface myDragBegan)
    registerComponent "DragEnded"     myDragEnded     (newComponentInterface myDragEnded)
    registerComponent "DragContinues" myDragContinues (newComponentInterface myDragContinues)
    registerComponent "DragOverride"  myDragOverride  (newComponentInterface myDragOverride)

beginDrag :: EntityID -> EntityID -> ECSMonad ()
beginDrag handEntityID draggedID = do
    startM44 <- getEntityPose handEntityID
    setEntityComponent myActiveDrag (ActiveDrag handEntityID startM44) draggedID

    inEntity draggedID $
        withComponent_ myDragBegan $ \dragBegan ->
            runUserFunctionProtected myDragBegan dragBegan

continueDrag :: HandEntityID -> ECSMonad ()
continueDrag draggingHandEntityID = do
    forEntitiesWithComponent myActiveDrag $ \(entityID, ActiveDrag handEntityID startM44) ->
        when (handEntityID == draggingHandEntityID) $ do
            currentM44 <- getEntityPose handEntityID
            let dragDistance = currentM44 `subtractMatrix` startM44

            inEntity entityID $
                withComponent_ myDragContinues $ \drag ->
                    runUserFunctionProtected myDragContinues (drag dragDistance)

endDrag :: HandEntityID -> ECSMonad ()
endDrag endingDragHandEntityID = do
    forEntitiesWithComponent myActiveDrag $ \(entityID, ActiveDrag handEntityID _) -> do
        when (handEntityID == endingDragHandEntityID) $ do
            removeEntityComponent myActiveDrag entityID

            inEntity entityID $
                withComponent_ myDragEnded $ \dragEnded ->
                    runUserFunctionProtected myDragEnded dragEnded
