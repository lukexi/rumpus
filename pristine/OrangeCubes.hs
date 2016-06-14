module OrangeCubes where
import Rumpus

start :: Start
start = do
    let n = 10
    let things = [V3 x (-10) z | x <- [-n..n], z <- [-n..n]]
    forM_ (zip [0..] things) $ \(i, pos) -> spawnChild $ do
        let V3 x y z = (*10) . fromIntegral <$> pos
        myPose  ==> position (V3 x y z)
        myShape ==> Cube
        mySize  ==> 10
        myTransformType ==> AbsolutePose
        myUpdate ==> do
           now <- (*2) <$> getNow
           let y' = y + sin (now + x + z) + sin (now + z)
           setPose (position (V3 x (0.1*y') z))
           setColor (colorHSL ((y')/20) 0.5 0.5)
