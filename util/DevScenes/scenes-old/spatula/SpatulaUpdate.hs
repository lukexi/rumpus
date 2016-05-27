    {-# LANGUAGE LambdaCase #-}
module SpatulaUpdate where
import Rumpus

update :: Update
update entityID = do
    now <- getNow
    let a     = (*5) . sin . (/10) $ now
        spatX = (*a) . sin  $ now
        spatZ = (*a) . cos  $ now
        newPose_ = Pose (V3 spatX 0.5   spatZ) (axisAngle (V3 0 1 0) (now + (pi/2)))
    setEntityPose entityID newPose_

    withState entityID $ \channel -> do
        (liftIO . atomically . readTChan) channel >>= \case
            Atom (Float freq) -> setEntityColor (colorHSL (freq/1000) 0.9 0.8) entityID
            _ -> return ()

