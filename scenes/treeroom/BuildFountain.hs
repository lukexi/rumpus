{-# LANGUAGE FlexibleContexts #-}
module BuildTree where
import Rumpus

createNewTimer = liftIO $ registerDelay (500 * 1000)

checkTimer = liftIO . atomically . readTVar

start :: OnStart
start = do
    removeChildren

    --parentID <- ask

    cmpOnUpdate ==> withScriptData (\timer -> do
        shouldSpawn <- checkTimer timer
        if shouldSpawn 
            then do
                pose <- getPose
                childID <- spawnEntity Transient $ do
                    cmpPose ==> pose & translation +~ (V3 0 0.3 0)
                    cmpShapeType ==> SphereShape
                    cmpSize ==> 0.1
                runEntity childID $ do
                    setLifetime 10
                    applyForce (V3 0 5 0)
                editScriptData $ \_ -> createNewTimer
            else return ())

    timer <- createNewTimer
    return (Just (toDyn timer))
