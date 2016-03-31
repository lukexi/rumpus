-- Note: run updae ticks in exception handler 
module DefaultStart where
import Rumpus

-------------------
-- MORE COMING SOON
-- AFTER I FIX THESE
-- ROMAIPUS
--------------------

keyNames = 
    [ "1234567890"
    , "qwertyuiop"
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
        
    thisID <- ask
    let handsWithIDs = [ (LeftHand, leftHandID)
                       , (RightHand, rightHandID)
                       ]
        eventDestinationID = thisID
    forM_ handsWithIDs $ \(whichHand, handID) -> do
        runEntity handID removeChildren
        spawnKeysForHand whichHand handID eventDestinationID
    putStrLnIO "HELLO"
    cmpOnUpdate ==> (forM_ [LeftHand, RightHand] $ \whichHand -> do
        --putStrLnIO "updatte"
        withHandEvents whichHand $ \case
            HandButtonEvent HandButtonGrip ButtonDown -> do
                putStrLnIO "GRIPPP"
                withScriptData $ \case
                    Just pendingEvent -> do
                        putStrLnIO ("SENDING EVENT! " ++ show pendingEvent)
                        --sendInternalEvent (GLFWEvent (Key pendingEventKey _ keyState modifierKeyBools))
                        sendInternalEvent (GLFWEvent pendingEvent)
                        setScriptData (Nothing :: Maybe Event)
                    Nothing -> do
                        putStrLnIO "No event :*((("
                        return ()
            _ -> return ())
    return Nothing

spawnKeysForHand whichHand handID eventDestinationID = do
    forM_ (zip [0..] keyNames) $ \(y, keyRow) -> do
        let numKeys = fromIntegral (length keyRow)
        forM_ (zip [0..] keyRow) $ \(x, keyName) -> do
            void $ spawnEntity Transient $ 
                makeKeyboardKey whichHand handID eventDestinationID x y numKeys keyName

inRect x y w h (V2 ptX ptY) =
    ptX > x && ptX < x + w && ptY > y && ptY < y + h

makeKeyboardKey whichHand parentHandID eventDestinationID x y numKeys keyName = do
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
        colorOn = hslColor 0.2 0.8 0.8
        colorOff = hslColor 0.3 0.8 0.4
    cmpText                   ==> [keyName]
    cmpTextPose               ==> mkTransformation 
                                      (axisAngle (V3 1 0 0) (-pi/2)) (V3 0 1 0)
    cmpColor                  ==> colorOff
    cmpParent                 ==> parentHandID
    cmpShapeType              ==> CubeShape
    cmpPhysicsProperties      ==> [NoPhysicsShape]
    cmpPose                   ==> (identity & translation .~ pose)
    cmpSize                   ==> V3 keyWidth 0.02 keyHeight
    cmpInheritParentTransform ==> InheritPose
    cmpOnUpdate ==> do
        withHandEvents whichHand $ \case
            HandStateEvent hand -> do
                let handXY = (hand ^. hndXY & _y *~ (-1)) + 0.5
                    isInKey = pointIsInKey handXY
                    color = if isInKey then colorOn else colorOff
                cmpColor ==> color
                when isInKey $ do
                    runEntity eventDestinationID $ 
                        setScriptData (Just (Character keyName))
            
            _ -> return ()