module Ribbon where
import Rumpus

start :: Start
start = do
    totSpeedKnob <- addKnob "Total Speed" (Linear 0 5) 1
    rotSpeedKnob <- addKnob "Rot Speed" (Linear 0 5) 1
    yScaleKnob   <- addKnob "YScale"   (Linear 0 10) 1
    zScaleKnob   <- addKnob "ZScale"   (Linear 0.1 10) 1
    forM_ [0..100] $ \i -> spawnChild $ do
        myShape         ==> Cube
        myTransformType ==> AbsolutePose
        myUpdate        ==> do
            now <- getNow
            totSpeed  <- readKnob totSpeedKnob
            rotSpeed <- readKnob rotSpeedKnob
            yScale <- readKnob yScaleKnob
            zScale <- readKnob zScaleKnob
            let n = now * totSpeed + (i * 0.05)
                y = yScale * sin n
                x = cos n * 5
            setPositionRotationSize
                (V3 x y (-10))
                (axisAngle (V3 1 0 1) (n*rotSpeed))
                (V3 9 0.01 (sin n * zScale))
            setColor (colorHSL (n*0.1) 0.8 0.6)
