module CubeSea where
import Rumpus

start :: Start
start = do
    baseHueKnob <- addKnob "Base Hue" (Linear 0 1) 0
    
    let n = 8
        size = 10
    let things = [V2 x z | x <- [-n..n], z <- [-n..n]]
    forM_ (zip [0..] things) $ \(i, pos) -> spawnChild $ do
        let V2 x z = (*size) . fromIntegral <$> pos
        myTransformType ==> AbsolutePose
        myPose          ==> position (V3 x (-size) z)
        myShape         ==> Cube
        mySize          ==> realToFrac size
        myUpdate ==> do
           baseHue <- getKnobValue baseHueKnob
           now <- (*1) <$> getNow
           let n = sin (now + x/10) + sin (now + z/9)
           let y = -size + n
               hue = baseHue + n * 0.1
           setPose (positionRotation 
               (V3 x y z)
               (axisAngle (V3 1 1 1) (n/9)))
           setColor (colorHSL hue 0.5 0.5)