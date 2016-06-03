module Rumpus.Systems.Hands where
import Rumpus.Systems.Controls
import Rumpus.Systems.Physics
import Rumpus.Systems.Collisions
import Rumpus.Systems.Shared
import Rumpus.Systems.Attachment
import PreludeExtra
import qualified Data.List as List

type HandEntityID = EntityID

data HandsSystem = HandsSystem
    { _hndLeftHand  :: HandEntityID
    , _hndRightHand :: HandEntityID
    , _hndBeams     :: Map WhichHand EntityID
    --, _hndHead      :: EntityID
    } deriving Show
makeLenses ''HandsSystem

defineSystemKey ''HandsSystem

startHandsSystem :: (MonadState ECS m, MonadIO m) => m ()
startHandsSystem = do
    let handColor = V4 0.6 0.6 0.9 1

        makeHand whichHand = do
            myColor           ==> handColor
            mySize            ==> V3 0.075 0.075 0.25
            myShape           ==> Cube
            myProperties      ==> [Floating, Ghostly, Ungrabbable]
            myMass            ==> 0
            myCollisionStart  ==> \_ impulse -> do
                hapticPulse whichHand (floor $ impulse * 10000)

    leftHandID  <- spawnEntity $ makeHand LeftHand
    rightHandID <- spawnEntity $ makeHand RightHand

    registerSystem sysHands $ HandsSystem
            { _hndLeftHand  = leftHandID
            , _hndRightHand = rightHandID
            , _hndBeams = mempty
            }

    return ()



getLeftHandID :: (MonadState ECS m) => m EntityID
getLeftHandID  = viewSystem sysHands hndLeftHand

getRightHandID :: (MonadState ECS m) => m EntityID
getRightHandID = viewSystem sysHands hndRightHand

getHandID :: MonadState ECS m => WhichHand -> m EntityID
getHandID whichHand = case whichHand of
    LeftHand  -> getLeftHandID
    RightHand -> getRightHandID

getHandIDs :: (MonadState ECS m) => m [(WhichHand, EntityID)]
getHandIDs = do
    leftHandID  <- getLeftHandID
    rightHandID <- getRightHandID
    return [ (LeftHand, leftHandID)
           , (RightHand, rightHandID)
           ]


otherHandFrom :: WhichHand -> WhichHand
otherHandFrom whichHand = case whichHand of
    LeftHand  -> RightHand
    RightHand -> LeftHand

getOtherHandID :: MonadState ECS m => WhichHand -> m EntityID
getOtherHandID whichHand = getHandID (otherHandFrom whichHand)



withLeftHandEvents :: MonadState ECS m => (HandEvent -> m ()) -> m ()
withLeftHandEvents f = withSystem_ sysControls $ \controlSystem -> do
    let events = controlSystem ^. ctsEvents
    forM_ events (\e -> onLeftHandEvent e f)

withRightHandEvents :: MonadState ECS m => (HandEvent -> m ()) -> m ()
withRightHandEvents f = withSystem_ sysControls $ \controlSystem -> do
    let events = controlSystem ^. ctsEvents
    forM_ events (\e -> onRightHandEvent e f)

withHandEvents :: MonadState ECS m => WhichHand -> (HandEvent -> m ()) -> m ()
withHandEvents hand f = withSystem_ sysControls $ \controlSystem -> do
    let events = controlSystem ^. ctsEvents
    forM_ events (\e -> onHandEvent hand e f)


isEntityBeingHeldByHand entityID whichHand = do
    handID <- getHandID whichHand
    isEntityAttachedTo entityID handID

isBeingHeldByHand whichHand = do
    entityID <- ask
    isEntityBeingHeldByHand entityID whichHand


isBeingHeld :: (MonadReader EntityID m, MonadState ECS m) => m Bool
isBeingHeld = isEntityBeingHeld =<< ask

isEntityBeingHeld entityID = do
    or <$> forM [LeftHand, RightHand] (isEntityBeingHeldByHand entityID)


