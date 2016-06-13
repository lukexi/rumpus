module Ribbon where
import Rumpus

start :: Start
start = do
    speedKnob  <- addKnob "Speed" (Linear 0 5) 1
    yScaleKnob <- addKnob "YScale"   (Linear 0 10) 1
    forM_ [0..100] $ \i -> spawnChild $ do
        myShape         ==> Cube
        myTransformType ==> AbsolutePose
        myUpdate        ==> do
            now <- getNow
            speed  <- getKnobValue speedKnob
            yScale <- getKnobValue yScaleKnob
            let n = now * speed + (i * 0.05)
                y = yScale * sin n
                x = cos n * 5
            setPositionRotationSize
                (V3 x y (-10))
                (axisAngle (V3 1 0 1) n)
                (V3 9 0.01 (sin n))
            setColor (colorHSL (n*0.1) 0.8 0.6)
