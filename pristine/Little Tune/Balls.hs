module Balls where
import Rumpus

rate :: DiffTime
rate = 3

start :: Start
start = do

    setRepeatingAction (1/rate) $ do

        -- Spawn a ball
        pose <- getPose
        spawnChild_ $ do
            -- Spawn upwards from our parent's pose
            let parentRotation = pose ^. _m33
                offset = parentRotation !* V3 0 0.3 0
            myPose     ==> pose & translation +~ offset
            myShape    ==> Sphere
            myBody     ==> Physical
            mySize     ==> 0.08
            myMass     ==> 0.1
            myColor    ==> colorHSL 0.8 0.9 0.5
            myLifetime ==> 2
            myStart    ==> do
                applyForce (parentRotation !* V3 0 0.3 0)