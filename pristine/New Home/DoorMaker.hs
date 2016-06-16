
module DoorMaker where
import Rumpus

start :: Start
start = do
    sceneNames <- listScenes

    let sceneNamesWithNewScene = Nothing : map Just sceneNames
        numItems               = fromIntegral $ length sceneNamesWithNewScene
        
    forM_ (zip [0..] sceneNamesWithNewScene) $ \(i, maybeCodePath) -> do
        let iF = fromIntegral i / numItems
            hue = iF
            x = fromIntegral $ i `div` 2 * 2
            z = fromIntegral $ i `mod` 2 * 4 - 2
            r = if i `mod` 2 == 0 then 0 else -pi
            platShift = if i `mod` 2 == 0 then 1 else -1
        spawnChild $ do
            myBody ==> Animated
            myBodyFlags ==> [Teleportable]
            myShape ==> Cube
            myPose ==> position (V3 x -0.05 (z + platShift))
            mySize ==> V3 2 0.1 2
            myColor ==> colorHSL hue 0.5 0.5
        spawnChild $ do
            myBody          ==> Animated
            myStartExpr     ==> ("Door.hs", "start")
            myPose          ==> positionRotation 
                (V3 x 1 z)
                (axisAngle (V3 0 1 0) r)
            setState (maybeCodePath, hue::Float) 
    
 