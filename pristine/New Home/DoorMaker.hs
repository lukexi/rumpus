
module DoorMaker where
import Rumpus

start :: Start
start = do
    sceneNames <- listScenes

    let sceneNamesWithNewScene = Nothing : map Just sceneNames
        numItems               = length sceneNamesWithNewScene
        
    forM_ (zip [0..] sceneNamesWithNewScene) $ \(i, maybeCodePath) -> do
        let hue = fromIntegral i / fromIntegral numItems
        
        spawnChild $ do
            myStartExpr   ==> ("Door.hs", "start")
            myCodeHidden  ==> True
            myPose        ==> position (V3 (fromIntegral i) 0 0)
            setState maybeCodePath 
    
 