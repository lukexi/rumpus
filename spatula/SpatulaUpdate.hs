{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Wobble where
import Rumpus

update :: OnUpdate
update entityID = do
    now <- getNow
    let a     = (*5) . sin . (/10) $ now
        spatX = (*a) . sin  $ now
        spatZ = (*a) . cos  $ now
        newPose_ = Pose (V3 spatX 0.1   spatZ) (axisAngle (V3 0 1 0) (now + (pi/2)))
    setEntityPose newPose_ entityID

    traverseM_ (use (wldComponents . cmpScriptData . at entityID)) $ \scriptData -> do
        case fromDynamic scriptData of
            Just channel -> (liftIO . atomically . readTChan) channel >>= \case
                Atom (Float freq) -> setEntityColor (hslColor (freq/1000) 0.9 0.8 1) entityID
                _ -> return ()
            Nothing -> return ()

