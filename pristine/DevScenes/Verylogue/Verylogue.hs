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
    myParent            ==> parentID
    myPhysicsProperties ==> [Kinematic]
    myPose              ==> (identity & translation . _x .~ x)
    mySize              ==> 0.5
    myOnCollisionStart  ==> \_ _ -> do
        hue <- liftIO randomIO
        myColor ==> hslColor hue 0.8 0.4
        sendEntityPd parentID "piano-key" (List [fromIntegral note, 1])
    myOnCollisionEnd    ==> \_ -> do
        sendEntityPd parentID "piano-key" (List [fromIntegral note, 0])
