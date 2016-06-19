module HandHelp where
import Rumpus

start :: Start
start = do
    helperHandID <- makeHand LeftHand
    thisID <- ask
    inEntity helperHandID $ do
        setPosition (V3 0 1 0)
        setRotation (V3 1 0 0) (pi/2)
        setParent thisID

        spawnChild $ do
            myPose ==> positionRotation (V3 1 0.2 0) (axisAngle (V3 1 0 0) (-pi/2))
            myText ==> "<- Open Object Library"

        spawnChild $ do
            myPose ==> positionRotation (V3 0 0.1 0) (axisAngle (V3 1 0 0) (-pi/2))
            myText ==> "<- Teleport (when platforms available)"
    return ()
