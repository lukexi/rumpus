
module DoorMaker where
import Rumpus

start :: Start
start = do
    sceneNames <- listScenes

    let sceneNamesWithNewScene = Nothing : map Just sceneNames
        numItems               = fromIntegral $ length sceneNamesWithNewScene
        tileColors             = cycle [0,1,1,0]
    forM_ (zip3 [0..] tileColors sceneNamesWithNewScene) $ \(i, t, mSceneName) -> do
        let hue       = fromIntegral i / numItems
            xI        = i `div` 2
            zI        = i `mod` 2
            x         = fromIntegral xI * 2
            z         = fromIntegral zI * 4 - 2
            r         = if zI == 0 then 0 else -pi
            platShift = if zI == 0 then 1 else -1
            tileColor = if t == 0 then 1 else 0.1

        spawnChild $ do
            myBody      ==> Animated
            myBodyFlags ==> [Teleportable]
            myShape     ==> Cube
            myPose      ==> position (V3 x -0.05 (z + platShift))
            mySize      ==> V3 2 0.1 2
            myColor     ==> tileColor
        spawnChild $ do
            myBody          ==> Animated
            myStartExpr     ==> ("Door.hs", "start")
            myPose          ==> positionRotation
                (V3 x 1 z)
                (axisAngle (V3 0 1 0) r)
            setState (mSceneName, hue::Float)


