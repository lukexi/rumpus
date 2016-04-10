{-# LANGUAGE FlexibleContexts #-}
module DroneSpawner where
import Rumpus

numDrones = 30

start :: OnStart
start = do
    removeChildren
    parentID <- ask
    forM [0..numDrones] $ \ i -> do
        childID <- spawnEntity $ do
            myParent ==> parentID
            myShapeType ==> SphereShape
            myPhysicsProperties ==> [Kinematic]
            mySize ==> 0.2
            myMass ==> 0.1
            let h = (fromIntegral $ (i * 27) `mod` 37) / 37
            myColor ==> hslColor h 0.9 0.8
            myOnUpdate ==> do
                now <- getNow
                let iF = fromIntegral i
                    rate = 0.05
                    t = rate * iF * now
                    x = 1 + 0.3 * iF * cos t
                    y = 1 + iF * 0.4
                    z = 1 + 0.3 * iF * sin t
                setPose (identity & translation .~ V3 x y z)
            myOnCollisionStart ==> \_ _ -> do
                myColor ==> hslColor 1 0.5 0.5

        return ()
    return Nothing