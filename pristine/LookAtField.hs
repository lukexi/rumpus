module LookAtField where

start :: Start
start = do
    let n = 5
        m = [0..n]
        positions = [ V3 x y z - fromIntegral n / 2 | x <- m, y <- m, z <- m ]
    forM_ positions $ \pos -> do
        spawnChild $ do
            myPose        ==> position pos
            myShape       ==> Cube
            mySize        ==> 0.01
            myProperties  ==> [Holographic]
            myColor       ==> colorHSL
                (fromIntegral e/2) 0.7 (fromIntegral e)
            myUpdate ==> do
                leftHandPos <- view translation <$> (getPose =<< getLeftHandID)
                setPose $ position pos !*! lookAt pos leftHandPos (V3 0 1 0)
