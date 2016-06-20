module HandHelp where
import Rumpus

makeHand2 whichHand = do
    handID <- spawnEntity $ do
        myColor           ==> handColor * 0.8
        mySize            ==> handSize
        myShape           ==> Cube
        myBody            ==> Detector
    -- Create a "wrist" below the hand
    spawnChildOf_ handID $ do
        myTransformType   ==> RelativeFull
        myColor           ==> handColor
        myShape           ==> Cube
        mySize            ==> V3 0.9 0.9 2
        myPose            ==> position (V3 0 0 1)
    -- App Button
    spawnChildOf_ handID $ do
        myTransformType   ==> RelativeFull
        myColor           ==> handColor * 0.5
        myShape           ==> Cube
        mySize            ==> V3 0.1 0.1 0.1
        myPose            ==> position (V3 0 -1 0.6)
    -- Grip buttons
    spawnChildOf_ handID $ do
        myTransformType   ==> RelativeFull
        myColor           ==> handColor * 0.5
        myShape           ==> Cube
        mySize            ==> V3 1.1 0.5 0.5
        myPose            ==> position (V3 0 0 1)
    return handID

connectEntities fromID toID = spawnChild $ do
    myShape         ==> Cube
    myTransformType ==> AbsolutePose
    --myStart ==> animateSizeInFrom0 0.5
    myUpdate ==> do
        fromPos <- view translation <$> getEntityWorldPose fromID
        toPos   <- view translation <$> getEntityWorldPose toID

        let (pose, size) = transformForLineFromTo fromPos toPos 0.01
        setPose pose
        setSize size

getEntityWorldPose :: MonadState ECS m => EntityID -> m (M44 GLfloat)
getEntityWorldPose entityID = inEntity entityID getWorldPose

getWorldPose :: (MonadState ECS m, MonadReader EntityID m) => m (M44 GLfloat)
getWorldPose = getWorldPoseScaled False

getWorldPoseScaled :: (MonadState ECS m, MonadReader EntityID m) => Bool -> m (M44 GLfloat)
getWorldPoseScaled scaled = do
    maybeParent <- getParent
    pose <- if scaled then getScaledPose else getPose
    case maybeParent of
        Nothing -> return pose
        Just parentID -> do
            transformType <- getTransformType
            case transformType of
                AbsolutePose -> return pose
                RelativePose -> do
                    parentPose <- inEntity parentID (getWorldPoseScaled False)
                    return (parentPose !*! pose)
                RelativeFull -> do
                    parentPose <- inEntity parentID (getWorldPoseScaled True)
                    return (parentPose !*! pose)


transformForLineFromTo fromPos toPos thickness =
    let center = fromPos + ((toPos - fromPos) / 2)
        pose   = orientToward center toPos (V3 0 1 0)
        size   = V3 thickness thickness (distance fromPos toPos)
    in (pose, size)

start :: Start
start = do
    helperHandID <- makeHand2 LeftHand
    thisID <- ask
    inEntity helperHandID $ do
        setPosition (V3 0 1 0)
        setRotation (V3 1 0 0) (pi/2)
        setParent thisID
    appButtonText <- spawnChild $ do
        myPose ==> positionRotation (V3 1 0.2 0) (axisAngle (V3 1 0 0) 0)
        mySize ==> 0.1
        myText ==> "Open Object Library"

    gripButtonText <- spawnChild $ do
        --myPose ==> positionRotation (V3 0 0.1 0) (axisAngle (V3 1 0 0) 0)
        myPose ==> positionRotation (V3 0 2 0) (axisAngle (V3 1 0 0) 0)
        mySize ==> 0.1
        myText ==> "Teleport (when platforms available)"


    tiny <- spawnChildOf gripButtonText $ do
        myTransformType ==> RelativeFull
        myShape ==> Cube
        mySize ==> 0.2
        myPose ==> positionRotation (V3 0 1 0)
            (axisAngle (V3 0 1 1) 1)
    --poseTest <- inEntity tiny getWorldPose
    --spawnChild $ do
    --    myPose ==> poseTest
    --    myShape ==> Cube
    --    myColor ==> V4 0 1 0 1
    --    mySize ==> 0.2
    --    myTransformType ==> AbsolutePose

    leftID <- getRightHandID
    connectEntities appButtonText tiny

    return ()
