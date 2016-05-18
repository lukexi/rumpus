module Sphere where
import Rumpus

-- Golden Section Spiral
-- (via http://www.softimageblog.com/archives/115)
-- Returns an even distribution of the requested
-- number of points on a sphere
pointsOnSphere (fromIntegral -> n) =
    let inc = pi * (3 - sqrt 5)
        off = 2 / n
    in flip map [0..n] $ \k ->
        let y   = k * off - 1 + (off / 2)
            r   = sqrt (1 - y*y)
            phi = k * inc
        in V3 (cos phi * r) y (sin phi * r)

start :: Start
start = do

    rootEntityID <- ask

    let numPoints = 30 :: Int
        sphere = pointsOnSphere numPoints
        hues = map ((/ fromIntegral numPoints) . fromIntegral) [0..numPoints]
    forM_ (zip sphere hues) $ \(pos, hue) -> void $ spawnEntity $ do
        myParent           ==> rootEntityID
        myPose             ==> translateMatrix pos
        myShape            ==> Sphere
        myProperties       ==> [Holographic]
        myInheritTransform ==> InheritFull
        mySize             ==> V3 0.5 0.5 0.5
        myColor            ==> colorHSL hue 0.8 0.5
        myUpdate ==> do
            now <- sin <$> getNow
            setSize (realToFrac ((sin now + 1) * 0.1 + 0.01))
