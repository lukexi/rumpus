module Ribbon where
import Rumpus

start :: Start
start = do
    forM_ [0..100] $ \i -> spawnChild $ do
        myShape      ==> Cube
        myProperties ==> [Holographic]
        myUpdate     ==> do
            now <- getNow
            let n = now * 1 + (i * 0.05)
                y = 5 * sin n
            setPositionRotationSize
                (V3 (cos n * 5) y (-10))
                (axisAngle (V3 1 0 1) n)
                (V3 9 0.01 (sin n))
            setColor (colorHSL (n*0.1) 0.8 0.6)
