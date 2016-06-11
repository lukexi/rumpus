module Tree where
import Rumpus

depth :: Num a => a
depth = 5


start :: Start
start = do
    setSize (V3 0.4 0.4 0.1)

    hueShift <- randomRange (0,1)
    let branch parentID n pos rot = do
            let scale = realToFrac $ 1 / (2^n)
                hue = (hueShift + fromIntegral n/9)
            childID <- spawnEntity $ do
                myParent           ==> parentID
                myPose             ==> positionRotation
                                        pos
                                        (axisAngle (V3 0 0 1) 0.3)
                myShape            ==> Cube
                mySize             ==> 0.001
                myColor            ==> colorHSL hue 0.8 0.5
                myUpdate           ==> do
                    now <- sin <$> getNow
                    let V3 pX pY pZ = pos
                    setPose $ positionRotation
                        (V3 pX pY pZ)
                        (axisAngle (V3 1 1 0) (now*2) * rot)
            inEntity childID $ do
                animateSizeTo (V3 0.1 0.5 0.1 * scale) 1
                when (n < depth) $ do
                    setDelayedAction 1 $ do
                        branch childID (n + 1) (V3  0.15 0.3 0 * scale) (axisAngle (V3 0 0 1) -1)
                        branch childID (n + 1) (V3 -0.15 0.3 0 * scale) (axisAngle (V3 0 0 1)  1)
    -- Unscaled root entity so that first branch is not scaled
    rootEntityID <- spawnChild (return ())
    branch rootEntityID (0::Int) (V3 0 0.5 0) (axisAngle (V3 0 0 1) 0)
