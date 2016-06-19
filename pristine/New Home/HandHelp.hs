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
        myColor           ==> handColor
        myShape           ==> Cube
        mySize            ==> V3 0.9 0.9 2
        myPose            ==> position (V3 0 0 1)
        myTransformType   ==> RelativeFull
    spawnChildOf_ handID $ do
        myTransformType   ==> RelativeFull
        myColor           ==> handColor * 0.5
        myShape           ==> Cube
        mySize            ==> V3 0.1 0.1 0.1
        myPose            ==> position (V3 0 -1 0.6)
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
        fromPos <- getEntityPosition fromID
        toPos   <- getEntityPosition toID

        let (pose, size) = transformForLineFromTo fromPos toPos 0.01
        setPose pose
        setSize size


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


    leftID <- getRightHandID
    connectEntities leftID helperHandID

        --spawnChild $ do
        --    myPose ==> positionRotation (V3 1 0.2 0) (axisAngle (V3 1 0 0) (-pi/2))
        --    myText ==> "<- Open Object Library"

        --spawnChild $ do
        --    myPose ==> positionRotation (V3 0 0.1 0) (axisAngle (V3 1 0 0) (-pi/2))
        --    myText ==> "<- Teleport (when platforms available)"
    return ()
