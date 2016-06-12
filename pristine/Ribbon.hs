module Ribbon where
import Rumpus

start :: Start
start = do
    speedKnob <- spawnActiveKnob "Speed" (Linear 0 5) 1 (const $ return ()) 
    barKnob   <- spawnActiveKnob "Bar" (Linear 0 5) 1 (const $ return ()) 
    thisID <- ask
    forM_ [0..100] $ \i -> spawnChild $ do
        myShape         ==> Cube
        myTransformType ==> AbsolutePose
        myUpdate        ==> do
            now <- getNow
            speed <- inEntity thisID $ getKnobData "Speed"
            bar <- inEntity thisID $ getKnobData "Bar"
            let n = now * speed + (i * 0.05)
                y = bar * 10 * sin n
            setPositionRotationSize
                (V3 (cos n * 5) y (-10))
                (axisAngle (V3 1 0 1) n)
                (V3 9 0.01 (sin n))
            setColor (colorHSL (n*0.1) 0.8 0.6)