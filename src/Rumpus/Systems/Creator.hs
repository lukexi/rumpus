module Rumpus.Systems.Creator where
import Rumpus.Systems.Hands

data CreatorSystem = CreatorSystem
    { _crtNewEntityID :: !(Maybe EntityID)
    }
makeLenses ''CreatorSystem
defineSystemKey ''CreatorSystem

initCreatorSystem :: MonadState ECS m => m ()
initCreatorSystem = do
    registerSystem sysCreator (CreatorSystem Nothing)


primeNewEntity whichHand = do
    handID <- getHandID whichHand

    newEntityID <- spawnEntity $ do
        myParent ==> handID
        myPose   ==> translateMatrix (V3 0 0 (-0.5))
        myUpdate ==> do
            now <- getNow
            setColor (colorHSL now 0.3 0.8)
        myDrag ==> do

