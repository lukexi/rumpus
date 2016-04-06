module Fountain where
import Rumpus

rate :: Float
rate = 2

majorScale = map (+60) [0,2,4,7,9]

start :: OnStart
start = do
    removeChildren

    cmpOnUpdate ==> withScriptData (\timer -> do
        shouldSpawn <- checkTimer timer
        if shouldSpawn 
            then do
                note <- randomFrom majorScale
                sendPd "note" (Atom $ realToFrac note)
                pose <- getPose
                childID <- spawnEntity Transient $ do
                    cmpPose ==> pose & translation +~ 
                        (pose ^. _m33) !* (V3 0 0.3 0)
                    cmpShapeType ==> SphereShape
                    cmpSize ==> 0.03
                    cmpMass ==> 0.1
                    cmpColor ==> hslColor (note / 12) 0.9 0.8
                runEntity childID $ do
                    setLifetime 10
                    applyForce $ (pose ^. _m33) !* (V3 0 0.3 0)
                editScriptData $ \_ -> createNewTimer rate
            else return ())

    timer <- createNewTimer rate
    return (Just (toDyn timer))