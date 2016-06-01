module MyObject6 where
import Rumpus

start :: Start
start = do

    initialPosition <- getPosition
    setState initialPosition
    myDrag ==> \_ -> do
        newPose <- getPose
        let newPosition = newPose ^. translation
        mLastBlot <- getState Nothing
        case mLastBlot of
            Just lastPosition
                | distance lastPosition newPosition > 0.05 -> do
                    setState newPosition
                    _newBlot <- spawnChild $ do
                        myPose       ==> newPose
                        myShape      ==> Cube
                        mySize       ==> 0.1
                        myProperties ==> [Holographic]
                        myColor      ==> colorHSL
                            (newPosition ^. _x) 0.7 0.8
                    return ()
            _ -> return ()
    return ()
