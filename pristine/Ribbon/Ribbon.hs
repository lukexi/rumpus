module Ribbon where
import Rumpus

start :: Start
start = do
    parentID <- ask
    addActiveKnob "Total Speed" (Linear -5 5) 1 setClockSpeed
    rotSpeedKnob <- addKnob "Rot Speed" (Exponential 0 5) 1
    yScaleKnob   <- addKnob "YScale"   (Linear 0 10) 1
    xScaleKnob   <- addKnob "XScale"   (Linear 0 1 ) 1
    zScaleKnob   <- addKnob "ZScale"   (Linear 0.1 10) 1
    forM_ [0..100] $ \i -> spawnChild $ do
        myShape         ==> Cube
        myTransformType ==> AbsolutePose
        myUpdate        ==> do
            now       <- getEntityClockTime parentID
            rotSpeed  <- readKnob rotSpeedKnob
            yScale    <- readKnob yScaleKnob
            zScale    <- readKnob zScaleKnob
            xScale    <- readKnob xScaleKnob
            let n = now + (i * 0.05)
                y = yScale * sin n
                x = cos n * 5 
            setPositionRotationSize
                (V3 x y (-15 ))
                (axisAngle (V3 1 0 1) (n*rotSpeed*rotSpeed))
                (V3 (9*xScale) 0.01 (sin n * zScale))
            setColor (colorHSL (n*0.1) 0.8 0.6)