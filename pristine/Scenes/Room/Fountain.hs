module Fountain where
import Rumpus

rate :: Float
rate = 0.5

majorScale = map (+60) [0,2,4,7,9]

start :: Start
start = do
    removeChildren

    myUpdate ==> withScriptData (\timer -> do
        shouldSpawn <- checkTimer timer
        if shouldSpawn 
            then do
                note <- randomFrom majorScale
                sendPd "note" (Atom $ realToFrac note)
                
                pose <- getPose
                spawnEntity $ do
                    myPose      ==> pose & translation +~ 
                                        (pose ^. _m33) !* (V3 0 0.3 0)
                    myShapeType ==> SphereShape
                    mySize      ==> 0.03
                    myMass      ==> 0.1
                    myColor     ==> hslColor (note / 12) 0.9 0.8
                    myStart     ==> do
                        setLifetime 10
                        applyForce $ (pose ^. _m33) !* (V3 0 0.3 0)
                now <- getNow
                newTimer <- createNewTimer rate
                setScriptData newTimer
            else return ())

    newTimer <- createNewTimer rate
    setScriptData newTimer
