module City where
import Rumpus


start :: Start
start = do


    let n = 5
        dim = 20
        height = dim * 5
        buildSites = [V3 (x * dim * 2) (-height) (z * dim * 2) | x <- [-n..n], z <- [-n..n]]
    heightKnob <- addKnob "Height" (Exponential 0.1 100) 1
    -- Ground
    spawnChild $ do
        let y = -height*2
        myPose          ==> position (V3 0 y 0)
        myShape         ==> Cube
        myTransformType ==> AbsolutePose
        mySize          ==> V3 (dim * n * 8) 1 (dim * n * 8)
        myColor         ==> colorHSL 0.7 0.6 0.2
    forM_ (zip [0..] buildSites) $ \(i, V3 x y z) -> do
        hue <- liftIO randomIO

        when (x /= 0 && z /= 0) $ void . spawnChild $ do
            myPose          ==> position (V3 x y z)
            myShape         ==> Cube
            myTransformType ==> AbsolutePose
            mySize          ==> V3 0 0 0
            myColor         ==> colorHSL hue 0.8 0.8
            myStart ==> do
                initialHeight <- getKnobValue heightKnob
                -- Animate each building in
                setDelayedAction (fromIntegral i*0.05) $ do
                    animateSizeFromTo 0 (V3 dim (abs x * initialHeight) dim) 0.5
                    -- Poll for height changes once animation has begun
                    myUpdate ==> do
                        newHeight <- getKnobValue heightKnob
                        setSize (V3 dim (abs x * newHeight) dim)
                        return ()