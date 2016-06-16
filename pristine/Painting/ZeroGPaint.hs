module ZeroGPaint where
import Rumpus
import qualified Data.Sequence as Seq

maxBlots = 300

start :: Start
start = do

    initialPosition <- getPosition
    setState (initialPosition, Seq.empty :: Seq EntityID)
    myDragContinues ==> \_ -> withState $ \(lastPosition, blots) -> do
        newPose <- getPose
        let newPosition = newPose ^. translation
        when (distance lastPosition newPosition > 0.05) $ do
            newBlot <- spawnChild $ do
                myPose          ==> newPose
                myShape         ==> Cube
                mySize          ==> 0.1
                myBody          ==> Physical
                myGravity       ==> 0
                myColor         ==> colorHSL
                    (newPosition ^. _x) 0.7 0.8

                myStart ==> setDelayedAction 1 $ do
                    myMass ==> 1
                    setBody Physical

            let (newBlots, oldBlots) = Seq.splitAt maxBlots blots
            forM_ oldBlots removeEntity
            setState (newPosition, newBlot <| newBlots)