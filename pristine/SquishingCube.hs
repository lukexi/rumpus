module SquishingCube where
import Rumpus

start :: Start
start = do
    spawnChild $ do
        myShape      ==> Cube
        myPose       ==> position (V3 0 0.5 0)
        mySize       ==> 0.2
        myUpdate     ==> do
            now <- getNow
            setRotation (V3 0 1 0) (now*5)
            setColor (colorHSL (sin (now*10) / 2 + 1) 0.4 0.5)
            setSize (V3 (0.2 * sin (now+1)) (0.2 * sin (now)) 0.2)

    return ()