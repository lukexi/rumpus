module AnimPaint where
import Rumpus
import qualified Data.Sequence as Seq

maxBlots = 1000


start :: Start
start = do

    initialPosition <- getPosition
    setState (initialPosition, Seq.empty :: Seq EntityID)
    myDragContinues ==> \_ -> withState $ \(lastPosition, blots) -> do
        newPose <- getPose
        let newPosition = newPose ^. translation
        when (distance lastPosition newPosition > 0.1) $ do
            let (newBlots, oldBlots) = Seq.splitAt maxBlots blots
            forM_ oldBlots removeEntity

            let i = fromIntegral (Seq.length newBlots) * 0.1
            newBlot <- spawnChild $ do
                myPose          ==> newPose
                myShape         ==> Cube
                mySize          ==> 0.1
                --myBody          ==> Animated
                myTransformType ==> AbsolutePose
                myColor         ==> colorHSL
                    (newPosition ^. _x) 0.7 0.8
                myUpdate ==> do
                    n <- (+ i) <$> getNow
                    let ySize = sin n * 0.2
                    setSize (V3 0.1 ySize 0.1)
                    setPose $ newPose !*! positionRotation 0
                        (axisAngle (V3 0 1 0) n)

            setState (newPosition, newBlot <| newBlots)
