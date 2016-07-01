module Stars where
import Rumpus

-- Golden Section Spiral (via http://www.softimageblog.com/archives/115)
pointsOnSphere :: Int -> [V3 GLfloat]
pointsOnSphere (fromIntegral -> n) =
    map (\k ->
        let y = k * off - 1 + (off / 2)
            r = sqrt (1 - y*y)
            phi = k * inc
        in V3 (cos phi * r) y (sin phi * r)
        ) [0..n]
    where inc = pi * (3 - sqrt 5)
          off = 2 / n

start :: Start
start = do

    -- We only take half the request points to get the upper hemisphere
    let numPoints = 400 :: Int
        points = reverse $ drop (numPoints `div` 2) $ pointsOnSphere numPoints
        hues = map ((/ fromIntegral numPoints) . fromIntegral) [0..numPoints]
    forM_ (zip3 [0..] points hues) $ \(i, pos, hue) -> spawnChild $ do
        myTransformType ==> AbsolutePose
        myShape         ==> Sphere
        mySize          ==> 0.001
        myColor         ==> colorHSL hue 0.8 0.8
        myPose          ==> position (pos * 500)
        myStart         ==> do
            setDelayedAction (fromIntegral i * 0.05) $
                setSize 5
        myUpdate ==> do
            now <- (fromIntegral i +) <$> getNow
            let offset = V3 (sin now * 50) 0 0
            setColor (colorHSL (now*0.5) 0.8 0.5)
            setPosition (offset + pos * 500)