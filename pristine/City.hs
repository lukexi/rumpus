module City where
import Rumpus

start :: Start
start = do
    let n = 5
        dim = 20
        height = dim * 5
        buildSites = [V3 (x * dim * 2) (-height) (z * dim * 2) | x <- [-n..n], z <- [-n..n]]
    -- Ground
    spawnChild $ do
        let y = -height*2
        myPose        ==> mkTransformation
                            (axisAngle (V3 0 0 1) 0) (V3 0 y 0)
        myShape       ==> Cube
        myProperties  ==> [Holographic]
        mySize        ==> V3 (dim * n * 8) 1 (dim * n * 8)
        myColor       ==> colorHSL 0.7 0.6 0.2
    forM_ buildSites $ \(V3 x y z) -> do
        hue <- liftIO randomIO

        when (x /= 0 && z /= 0) $ void . spawnChild $ do
            myPose       ==> mkTransformation
                                (axisAngle (V3 0 0 1) 0) (V3 x y z)
            myShape      ==> Cube
            myProperties ==> [Holographic]
            mySize       ==> V3 dim (abs x) dim
            myColor      ==> colorHSL hue 0.8 0.8
            -- myUpdate             ==> do
            --    let rate = 7000
            --    now <- getNow
            --    setSize $ V3 dim (height * (sin $ rate * now / (x * z))) dim

