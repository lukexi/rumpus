module LookAtField where
import Rumpus

start :: Start
start = do
    let n = 5
        m = [0..n]
        positions = [ V3 x (y+3) z - realToFrac n / 2 | x <- m, y <- m, z <- m ]
    forM_ positions $ \((/3) -> pos) -> do
        spawnChild $ do
            myPose          ==> position pos
            myShape         ==> Cube
            mySize          ==> V3 0.01 0.01 0.3
            myTransformType ==> AbsolutePose
            myColor         ==> colorHSL 0 0.7 0.9
            myUpdate ==> do
                leftHandPos <- view translation <$> (getEntityPose =<< getLeftHandID)
                let rotPos = inv44 $ lookAt pos leftHandPos (V3 0 1 0)
                setPose rotPos
                setColor (colorHSL (rotPos ^. _x . _x) 0.8 0.7)
