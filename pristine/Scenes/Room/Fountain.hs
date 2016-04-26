module Fountain where
import Rumpus

rate :: Float
rate = 5

majorScale = map (+60) [0,2,4,7,9]

start :: Start
start = do
    removeChildren

    myUpdate ==> do
        withState $ \timer -> do
            shouldSpawn <- checkTimer timer
            when shouldSpawn $ do
                -- Play a note
                note <- randomFrom majorScale
                sendPd "note" (Atom $ realToFrac note)
                
                -- Spawn a ball
                pose <- getPose
                spawnEntity $ do
                    myPose      ==> pose & translation +~ 
                                        (pose ^. _m33) !* (V3 0 0.3 0)
                    myShape ==> Sphere
                    mySize      ==> 0.03
                    myMass      ==> 0.1
                    myColor     ==> hslColor (note / 12) 0.9 0.8
                    myStart     ==> do
                        setLifetime 10
                        applyForce $ (pose ^. _m33) !* (V3 0 0.3 0)
                
                -- Create a new timer
                newTimer <- createNewTimer rate
                setState newTimer

    -- Create the initial timer
    newTimer <- createNewTimer rate
    setState newTimer