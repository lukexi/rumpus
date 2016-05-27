module Fountain where
import Rumpus

--rate :: Float
rate = 5

majorScale = map (+56) [0,2,4,7,9]

start :: Start
start = do
    setPdPatchFile "fountain-voice.pd"

    setRepeatingAction (1/rate) $ do
        -- Play a note
        note <- randomFrom majorScale
        sendPd "note" (realToFrac note)

        -- Spawn a ball
        pose <- getPose
        spawnEntity $ do
            -- Spawn upwards from our parent's pose
            let parentRotation = pose ^. _m33
                offset = parentRotation !* V3 0 0.3 0
            myPose  ==> pose & translation +~ offset
            myShape ==> Sphere
            mySize  ==> 0.03
            myMass  ==> 0.1
            myColor ==> colorHSL (note / 12) 0.9 0.8
            myStart ==> do
                setLifetime 2
                applyForce (parentRotation !* V3 0 0.3 0)
        return ()