module LoaderAmbient where
import Rumpus

start :: Start
start = do


    let n = 50
    forM (take 100 $ cycle "RUMPUSrumpus") $ \letter -> do
        pos <- V3 <$> randomRange (-n,n)
                  <*> randomRange (-n,n)
                  <*> randomRange (-n,n)

        let (V3 x y z) = pos
        unless (abs x < 4 && abs y < 4 && abs z < 4) $ do
            void . spawnChild $ do
                myPose ==> translateMatrix pos
                mySize ==> 0.2
                myText ==> [letter]
                myTextPose ==> translateMatrix (V3 0 1 0)
                myProperties ==> [Holographic]
                myShape ==> Cube
                myUpdate ==> do
                    now <- getNow
                    let n = (now + pos ^. _x + pos ^. _y) * 0.5
                    setSize (realToFrac (sin n))
                    setPose $ rotationAndPosition
                       (axisAngle pos n)
                       (pos & _x +~ sin n & _y +~ cos n)
                    setColor (colorHSL (x+(sin n * 0.3)) 0.5 0.5)

    return ()
