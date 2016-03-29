{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
-- Note: run updae ticks in exception handler 
module DefaultStart where
import Rumpus

-------------------
-- MORE COMING SOON
-- AFTER I FIX THESE
-- RECOMPILATION PAUSES
--------------------

keyNames = 
    [ "qwertyuiop"
    , "asdfghjkl;"
    , "zxcvbnm,./"
    ]

numRows = fromIntegral (length keyNames)

keyWidth = 0.05
keyHeight = 0.05
keyPad = 0.01
keyWidthT = keyWidth + keyPad
keyHeightT = keyHeight + keyPad

start :: OnStart
start = do
    vrPal <- viewSystem sysControls ctsVRPal

    removeChildren
    leftHandID  <- getLeftHandID
    rightHandID <- getRightHandID

    runEntity leftHandID $ do
        cmpOnCollisionStart  ==> \_ impulse -> do
            triggerHandHapticPulse vrPal LeftHand 0 (floor $ impulse * 10000)

    runEntity rightHandID $ do
        cmpOnCollisionStart  ==> \_ impulse -> 
            triggerHandHapticPulse vrPal RightHand 0 (floor $ impulse * 10000)
        

    let handsWithIDs = [ (LeftHand, leftHandID)
                       , (RightHand, rightHandID)
                       ]
    forM_ handsWithIDs $ \(whichHand, handID) -> do
      runEntity handID removeChildren
      spawnKeysForHand whichHand handID
    
    return Nothing

spawnKeysForHand whichHand handID = do
    forM_ (zip [0..] keyNames) $ \(y, keyRow) -> do
        let numKeys = fromIntegral (length keyRow)
        forM_ (zip [0..] keyRow) $ \(x, keyName) -> do
            void $ spawnEntity Transient $ 
                makeKeyboardKey whichHand handID x y numKeys keyName

inRect x y w h (V2 ptX ptY) =
    ptX > x && ptX < x + w && ptY > y && ptY < y + h

makeKeyboardKey whichHand parentID x y numKeys keyName = do
    let (xF, yF) = (fromIntegral x, fromIntegral y)
        keyProgX = xF / numKeys 
        keyProgY = yF / numRows
        keyProgW = 1 / numKeys
        keyProgH = 1 / numRows
        pointIsInKey = inRect keyProgX keyProgY keyProgW keyProgH
        keyX = keyOffsetX + xF * keyWidthT
        keyY = keyOffsetY + yF * keyHeightT
        keyOffsetX = -keyWidthT * pred numKeys / 2
        keyOffsetY = -0.2
        pose = V3 keyX 0.1 keyY
        colorOn = hslColor 0.1 0.8 0.8
        colorOff = hslColor 0.4 0.8 0.4
    cmpColor                  ==> colorOff
    cmpParent                 ==> parentID
    cmpShapeType              ==> CubeShape
    cmpPhysicsProperties      ==> [NoPhysicsShape]
    cmpPose                   ==> (identity & translation .~ pose)
    cmpSize                   ==> V3 keyWidth 0.02 keyHeight
    cmpInheritParentTransform ==> InheritPose
    cmpOnUpdate ==> do
        withHandEvents whichHand $ \case
            HandStateEvent hand -> do
                let handXY = (hand ^. hndXY & _y *~ (-1)) + 0.5
                    color = if pointIsInKey handXY then colorOn else colorOff
                cmpColor ==> color
            _ -> return ()