module Grass where
import Rumpus

start :: Start
start = do
    let n = 10
        m = [0..n]
        positions = [ V3 x 0 z - realToFrac n / 2 | x <- m, z <- m ]
    forM_ positions $ \((/3) -> pos) -> do
        spawnChild $ do
            myPose          ==> position pos
            myShape         ==> Cube
            mySize          ==> V3 0.01 0.01 0.3
            myTransformType ==> AbsolutePose
            myColor         ==> colorHSL 0 0.7 0.9
            myUpdate ==> do
                let hand = if floor (pos ^. _x) `mod` 2 == 0 then LeftHand else RightHand
                handPos <- view translation <$> getHandPose hand
                let rotPos = orientToward (pos & _y .~ 0) handPos (V3 0 1 0)
                setPose rotPos
                setColor (colorHSL (rotPos ^. _x . _x) 0.8 0.7)
