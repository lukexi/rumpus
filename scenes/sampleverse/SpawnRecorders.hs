{-# LANGUAGE FlexibleContexts #-}
module DefaultStart where
import Rumpus

start :: OnStart
start = do
    printIO "HELLOOOOo"
    rootEntityID <- ask
    let recorderAt x = do
            cmpPose ==> newPose & posPosition . _x .~ x
            cmpSize ==> 0.25
            cmpPdPatchFile ==> "recorder"
            cmpOnCollisionStart ==> \_ _ -> do
                sendPd "record-toggle" (Atom 1)
                hue <- liftIO randomIO
                setColor (hslColor hue 0.8 0.4 1)
            cmpOnCollisionEnd ==> \_ -> do
                sendPd "record-toggle" (Atom 0)
            cmpPhysicsProperties ==> [IsKinematic]
            cmpParent ==> rootEntityID
    forM_ [-1,-0.75..1] $ \x -> spawnEntity Transient $ recorderAt x
    return Nothing
