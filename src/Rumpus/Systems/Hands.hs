module Rumpus.Systems.Hands where
import Rumpus.Systems.Controls
import Rumpus.Systems.Physics
import Rumpus.Systems.Collisions
import Rumpus.Systems.Shared
import Rumpus.Systems.Attachment
import PreludeExtra
import RumpusLib

type HandEntityID = EntityID

data HandsSystem = HandsSystem
    { _hndLeftHand  :: HandEntityID
    , _hndRightHand :: HandEntityID
    , _hndBeams     :: Map WhichHand EntityID
    --, _hndHead      :: EntityID
    } deriving Show
makeLenses ''HandsSystem

defineSystemKey ''HandsSystem

handSize :: V3 GLfloat
handSize = V3 0.075 0.075 0.075

handColor :: V4 GLfloat
handColor = V4 0.6 0.6 0.9 1

makeHand :: (MonadIO m, MonadState ECS m, MonadBaseControl IO m)
         => WhichHand -> m EntityID
makeHand whichHand = do
    handID <- spawnEntity $ do
        myColor           ==> handColor
        mySize            ==> handSize
        myShape           ==> Cube
        myBody            ==> Detector
        myBodyFlags       ==> [Ungrabbable]
        myMass            ==> 0
        myCollisionBegan  ==> \_ impulse -> do
            hapticPulse whichHand (floor $ impulse * 10000)
    -- Create a "wrist" below the hand
    spawnChildOf_ handID $ do
        myColor           ==> handColor
        mySize            ==> V3 0.07 0.07 0.15
        myShape           ==> Cube
        myPose            ==> position (V3 0 0 0.1)
    return handID

startHandsSystem :: (MonadBaseControl IO m, MonadState ECS m, MonadIO m) => m ()
startHandsSystem = do

    leftHandID  <- makeHand LeftHand
    rightHandID <- makeHand RightHand

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

getWhichHand :: MonadState ECS m => EntityID -> m (Maybe WhichHand)
getWhichHand entityID = do
    leftHandID  <- getLeftHandID
    rightHandID <- getRightHandID
    return $ if
        | entityID == leftHandID  -> Just LeftHand
        | entityID == rightHandID -> Just RightHand
        | otherwise               -> Nothing


getHandIDs :: (MonadState ECS m) => m [EntityID]
getHandIDs = do
    leftHandID  <- getLeftHandID
    rightHandID <- getRightHandID
    return [ leftHandID
           , rightHandID
           ]

getHandIDsByWhichHand :: (MonadState ECS m) => m [(WhichHand, EntityID)]
getHandIDsByWhichHand = do
    leftHandID  <- getLeftHandID
    rightHandID <- getRightHandID
    return [ (LeftHand, leftHandID)
           , (RightHand, rightHandID)
           ]

getHandPose :: MonadState ECS m => WhichHand -> m (M44 GLfloat)
getHandPose whichHand = getEntityPose =<< getHandID whichHand

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

isEntityBeingHeldByHand :: MonadState ECS m
                           => EntityID -> WhichHand -> m Bool
isEntityBeingHeldByHand entityID whichHand = do
    handID <- getHandID whichHand
    isEntityAttachedTo entityID handID

isBeingHeldByHand :: (MonadState ECS m, MonadReader EntityID m)
                     => WhichHand -> m Bool
isBeingHeldByHand whichHand = do
    entityID <- ask
    isEntityBeingHeldByHand entityID whichHand


isBeingHeld :: (MonadReader EntityID m, MonadState ECS m) => m Bool
isBeingHeld = isEntityBeingHeld =<< ask

isEntityBeingHeld :: MonadState ECS f => EntityID -> f Bool
isEntityBeingHeld entityID = do
    or <$> forM [LeftHand, RightHand] (isEntityBeingHeldByHand entityID)


