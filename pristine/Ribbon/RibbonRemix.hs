module Ribbon where
import Rumpus

-- Devin Chalmers remix

start :: Start
start = do
    parentID <- ask
    addActiveKnob "Total Speed" (Linear -5 5) 1 setClockSpeed
    rotSpeedKnob <- addKnob "Rot Speed" (Linear 0 5) 1
    yScaleKnob   <- addKnob "YScale"   (Linear 0 10) 1
    zScaleKnob   <- addKnob "ZScale"   (Linear 0.1 10) 1
    forM_ [0..100] $ \i -> spawnChild $ do
        myShape         ==> Sphere
        myTransformType ==> AbsolutePose
        myUpdate        ==> do
            now       <- getEntityClockTime parentID
            rotSpeed  <- readKnob rotSpeedKnob
            yScale    <- readKnob yScaleKnob
            zScale    <- readKnob zScaleKnob
            let n = now + (i * 0.5)
                y = yScale * tan n/2
                x = sin n * 5
            setPositionRotationSize
                (V3 x y (-10))
                (axisAngle (V3 1 0 1) (n*rotSpeed))
                (V3 9 0.1 (sin n * zScale))
            setColor (colorHSL (n*0.1) 0.8 0.6)
