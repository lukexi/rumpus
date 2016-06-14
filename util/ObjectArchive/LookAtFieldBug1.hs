module LookAtField where
import Rumpus

start :: Start
start = do
    let n = 5
        m = [0..n]
        positions = [ V3 x y z - realToFrac n / 2 | x <- m, y <- m, z <- m ]
    forM_ positions $ \pos -> do
        spawnChild $ do
            myPose          ==> position pos
            myShape         ==> Cube
            mySize          ==> 0.01
            myTransformType ==> AbsolutePose
            myColor         ==> colorHSL
                0 0.7 0.9
            myUpdate ==> do
                leftHandPos <- view translation <$> (getEntityPose =<< getLeftHandID)
                setPose $ position pos !*! lookAt pos leftHandPos (V3 0 1 0)



