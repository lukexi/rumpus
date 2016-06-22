
module DoorMaker where
import Rumpus

tileW = 2
tileH = 0.1

start :: Start
start = do
    setPlayerPosition 0
    sceneNames <- listScenes

    let sceneNamesWithNewScene = map Just sceneNames ++ [Nothing]
        numItems               = fromIntegral $ length sceneNamesWithNewScene
        tileColors             = cycle [0,1,1,0]
    forM_ (zip3 [0..] tileColors sceneNamesWithNewScene) $ \(i, t, mSceneName) -> do
        let hue       = fromIntegral i / numItems
            zI        = i `div` 2
            xI        = i `mod` 2
            x         =  fromIntegral xI * 2 - tileW/2
            z         = -fromIntegral zI * 2 + tileW/2
            r         = if xI == 0 then pi/2 else -pi/2
            platShift = if xI == 0 then -1   else 1
            tileColor = if t  == 0 then 1    else 0.15
        spawnChild $ do
            myBody      ==> Animated
            myBodyFlags ==> [Teleportable]
            myShape     ==> Cube
            myPose      ==> position (V3 x (-tileH / 2) z)
            mySize      ==> V3 tileW tileH tileW
            myColor     ==> tileColor
        spawnChild $ do
            myBody          ==> Animated
            myStartCodeFile ==> ("Door.hs", "start")
            myPose          ==> positionRotation
                (V3 (x+platShift) 1 z)
                (axisAngle (V3 0 1 0) r)
            setState (mSceneName, hue::Float)


