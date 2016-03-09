{-# LANGUAGE FlexibleContexts #-}
module DefaultStart where
import Rumpus

start :: OnStart
start = do
    removeChildren
    rootEntityID <- ask

    forM_ [0..12] $ \n -> spawnEntity Transient $ pianokey rootEntityID n
    return Nothing

pianokey parentID n = do
    let note = n + 60
        x = fromIntegral n / 1.5 - 3
    cmpParent            ==> parentID
    cmpPhysicsProperties ==> [IsKinematic]
    cmpPose              ==> (identity & translation . _x .~ x)
    cmpSize              ==> 0.5
    cmpOnCollisionStart  ==> \_ _ -> do
        hue <- liftIO randomIO
        cmpColor ==> hslColor hue 0.8 0.4 1
        sendEntityPd parentID "piano-key" (List [fromIntegral note, 1])
    cmpOnCollisionEnd    ==> \_ -> do
        sendEntityPd parentID "piano-key" (List [fromIntegral note, 0])
