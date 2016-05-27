{-# LANGUAGE FlexibleContexts #-}
module Gun where
import Rumpus


majorScale = map (+60) [0,2,4,7,9]
randomNote = do
    i <- randomRange (0, length majorScale - 1)
    return (majorScale !! i)

start :: Start
start = do
    removeChildren

    setRepeatingAction (1/5) $ do
        note <- randomNote
        sendPd "note" (Atom $ realToFrac note)
        pose <- getPose
        childID <- spawnEntity $ do
            myPose ==> pose & translation +~
                (pose ^. _m33) !* (V3 0 0.3 0)
            myShape ==> Sphere
            mySize ==> 0.03
            myMass ==> 0.1
            myColor ==> colorHSL (note / 12) 0.9 0.8
        inEntity childID $ do
            setLifetime 10
            applyForce $ (pose ^. _m33) !* (V3 0 3.9 0)

