module Verylogue where
import Rumpus

majorScale = [0,2,4,5,7,9,11,12]

start :: OnStart
start = do
    removeChildren

    forM_ majorScale $ \n -> do
        let note = fromIntegral $ n + 60
        sendPd "piano-key" (List [note, 0])
    rootEntityID <- ask
    rootPose <- getPose
    forM_ (zip [0..] majorScale) $ \(i, note) -> do
        keyID <- spawnEntity Transient $ 
            makePianoKey rootEntityID rootPose i note
        attachEntity rootEntityID keyID False
    return Nothing

makePianoKey parentID parentPose i noteDegree = do
    let note = fromIntegral $ noteDegree + 60
        x = (1/12) * fromIntegral i - 0.27
        pose = V3 x 0.4 0
        hue  = fromIntegral i / fromIntegral (length majorScale)
        colorOn = hslColor hue 0.8 0.8
        colorOff = hslColor hue 0.8 0.4
    myColor ==> colorOff
    myParent            ==> parentID
    myShapeType         ==> CubeShape
    myPhysicsProperties ==> [Kinematic, NoContactResponse]
    myPose              ==> parentPose !*! (identity & translation .~ pose)
    mySize              ==> V3 0.01 0.2 0.3
    myOnCollisionStart  ==> \_ _ -> do
        myColor ==> colorOn
        sendEntityPd parentID "piano-key" (List [note, 1])
    myOnCollisionEnd    ==> \_ -> do
        myColor ==> colorOff
        sendEntityPd parentID "piano-key" (List [note, 0])