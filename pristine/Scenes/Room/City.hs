module City where
import Rumpus

-- Golden Section Spiral 
-- (via http://www.softimageblog.com/archives/115)
pointsOnSphere (fromIntegral -> n) = 
    let inc = pi * (3 - sqrt 5)
        off = 2 / n
    in flip map [0..n] $ \k ->
        let y = k * off - 1 + (off / 2)
            r = sqrt (1 - y*y)
            phi = k * inc
        in V3 (cos phi * r) y (sin phi * r)

start :: Start
start = do
    createBuildings
    createStars

createBuildings :: EntityMonad ()
createBuildings = do
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
        myProperties  ==> [NoPhysicsShape]
        mySize        ==> V3 (dim * n * 8) 1 (dim * n * 8)
        myColor       ==> hslColor 0.7 0.6 0.2
    forM_ buildSites $ \(V3 x y z) -> do
        hue <- liftIO randomIO
        
        when (x /= 0 && z /= 0) $ void . spawnChild $ do
            myPose       ==> mkTransformation 
                                (axisAngle (V3 0 0 1) 0) (V3 x y z)
            myShape      ==> Cube
            myProperties ==> [NoPhysicsShape]
            mySize       ==> V3 dim (abs x) dim
            myColor      ==> hslColor hue 0.8 0.8
            -- myUpdate             ==> do
            --    let rate = 7000
            --    now <- getNow
            --    setSize $ V3 dim (height * (sin $ rate * now / (x * z))) dim 

createStars :: EntityMonad ()
createStars = do
    
    let numPoints = 200 :: Int
        -- Only take the upper hemisphere
        sphere = drop (numPoints `div` 2) $ pointsOnSphere numPoints
        hues = map ((/ fromIntegral numPoints) . fromIntegral) [0..numPoints]
    forM_ (zip sphere hues) $ \(pos, hue) -> void . spawnChild $ do
        myPose       ==> mkTransformation 
                            (axisAngle (V3 0 0 1) 0.3) (pos * 1000)
        myShape      ==> Sphere
        myProperties ==> [NoPhysicsShape]
        mySize       ==> 5
        myColor      ==> hslColor hue 0.8 0.8
