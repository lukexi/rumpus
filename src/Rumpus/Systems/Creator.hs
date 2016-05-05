module Rumpus.Systems.Creator where
import Rumpus.Systems.Hands
import Rumpus.Systems.SceneEditor
import Rumpus.Systems.CodeEditor

data CreatorSystem = CreatorSystem
    { _crtNewEntityID :: !(Map WhichHand EntityID)
    }
makeLenses ''CreatorSystem
defineSystemKey ''CreatorSystem

initCreatorSystem :: MonadState ECS m => m ()
initCreatorSystem = do
    registerSystem sysCreator (CreatorSystem Nothing)


unprimeNewEntity whichHand = do
    mEntityID <- viewSystem sysCreator (crtNewEntityID . at whichHand)
    forM_ mEntityID $ \entityID -> do
        handID <- getHandID whichHand
        detachAttachedEntity handID entityID
        removeEntity entityID

primeNewEntity whichHand = do
    handID <- getHandID whichHand


    newEntityID <- spawnEntity $ do
        myShape       ==> Cube
        mySize        ==> 0.1
        myPose        ==> translateMatrix (V3 0 0 (-0.5))
        myUpdate      ==> do
            now <- getNow
            setColor (colorHSL now 0.3 0.8)
        -- It might be more elegant to use the exclusive-attachment system
        -- and just piggyback on SceneEditor's grabbing functionality here.
        myDragBegan ==> do
            removeComponent myDragBegan
            entityID <- ask
            DragFrom handEntityID _ <- getComponent myDragFrom
            handEntityID `grabEntity` entityID
            addCodeExpr "Start" "start" myStartExpr myStart
    modifySystemState sysCreator $ crtNewEntityID . at whichHand ?= newEntityID
