module Vine where
import Rumpus

start :: Start
start = do

    let nMax = 100
    let branch 0 = return ()
        branch n = do
            let hue = fromIntegral n / fromIntegral nMax
            [x,y,z,w] <- replicateM 4 (randomRange (0,1))
            child <- spawnChild $ do
                myShape ==> Cube
                myPose  ==> positionRotation (V3 0 0.4 0)
                    (axisAngle (V3 x y z) w)
                mySize  ==> V3 0.1 0.4 0.1
                myColor ==> colorHSL hue 0.8 0.8
                myUpdate ==> do
                    now <- (*0.1) <$> getNow
                    --setSize (V3 0.1 (sin now) 0.1)
                    setRotation (V3 x y z) (sin now * w)
            lift $ inEntity child $ branch (n - 1)
    branch nMax
    return ()
