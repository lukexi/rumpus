module Helix where
import Rumpus

start :: Start
start = do
    spawnChildren [0..100] $ \i -> do
        myShape ==> Cube
        mySize ==> V3 0.5 0.02 0.02
        myPose ==> positionRotation (V3 0 (0.5+i*0.1) 0)
            (axisAngle (V3 0 1 0) (i*0.2))
        myColor ==> colorHSL (i*0.2) 0.5 0.7
    return ()