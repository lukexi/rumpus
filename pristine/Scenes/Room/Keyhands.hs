q
module DefaultStart where
import Rumpus


(leftHandKeys, rightHandKeys) = 
    map fst &&& map snd $
        [ ("12345", "67890")
        , ("qwert", "yuiop")
        , ("asdfg", "hjkl;")
        , ("zxcvb", "nm,./")
        ]

keyWidth = 0.05
keyHeight = 0.05
keyPad = 0.01
keyWidthT = keyWidth + keyPad
keyHeightT = keyHeight + keyPad

-- How far up the controllers the keyboard appears
keyOffsetY = -0.2

start :: OnStart
start = do

    removeChildren
    leftHandID  <- getLeftHandID
    rightHandID <- getRightHandID
    
    vrPal <- viewSystem sysControls ctsVRPal
    runEntity leftHandID $ do
        cmpOnCollisionStart  ==> \_ impulse -> do
            triggerHandHapticPulse vrPal LeftHand 0 (floor $ impulse * 10000)


    runEntity rightHandID $ do
        cmpOnCollisionStart  ==> \_ impulse -> 
            triggerHandHapticPulse vrPal RightHand 0 (floor $ impulse * 10000)
    
    -- Have hands write their key events to this entityID
    -- so we can pass them along on click to the InternalEvents channel
    eventDestinationID <- ask
    let handsWithIDs = [ (LeftHand, leftHandID)
                       , (RightHand, rightHandID)
                       ]
        handsWithKeys = zip handsWithIDs [leftHandKeys, rightHandKeys]
    forM_ handsWithKeys $ \((whichHand, handID), keyRows) -> do
        runEntity handID removeChildren
        spawnKeysForHand whichHand handID keyRows 
    cmpOnUpdate ==> (forM_ handsWithIDs $ \(whichHand, handID) -> do
        withHandEvents whichHand $ \case
            HandButtonEvent HandButtonPad ButtonDown -> do
                runEntity handID $ withScriptData $ \case
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

spawnKeysForHand whichHand handID keyRows = do
    let numRows = fromIntegral (length keyRows)
        maxNumKeys = fromIntegral $ maximum (map length keyRows)
    void $ spawnEntity Transient $ makeThumbNub whichHand handID maxNumKeys numRows
    forM_ (zip [0..] keyRows) $ \(y, keyRow) -> do
        let numKeys = fromIntegral (length keyRow)
        forM_ (zip [0..] keyRow) $ \(x, keyName) -> do
            
            void $ spawnEntity Transient $ 
                makeKeyboardKey whichHand handID x y numKeys numRows keyName

inRect x y w h (V2 ptX ptY) =
    ptX > x && ptX < x + w && ptY > y && ptY < y + h

makeKeyboardKey whichHand parentHandID x y numKeys numRows keyName = do
    let (xF, yF) = (fromIntegral x, fromIntegral y)
        keyProgX = xF / numKeys 
        keyProgY = yF / numRows
        keyProgW = 1 / numKeys
        keyProgH = 1 / numRows
        pointIsInKey = inRect keyProgX keyProgY keyProgW keyProgH
        keyX = keyOffsetX + xF * keyWidthT
        keyY = keyOffsetY + yF * keyHeightT
        keyOffsetX = -keyWidthT * (numKeys - 1) / 2
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
                    runEntity parentHandID $ setScriptData (Just (Character keyName))
            _ -> return ()

makeThumbNub whichHand parentHandID maxNumKeys numRows = do
    let keyboardDims = V2 (maxNumKeys * keyWidthT) (numRows * keyHeightT)
        colorOn = hslColor 0.2 0.8 0.8
        colorOff = hslColor 0.3 0.8 0.4
    cmpColor                  ==> colorOn
    cmpParent                 ==> parentHandID
    cmpShapeType              ==> SphereShape
    cmpPhysicsProperties      ==> [NoPhysicsShape]
    cmpSize                   ==> realToFrac 0.02
    cmpInheritParentTransform ==> InheritPose
    cmpOnUpdate ==> do
        withHandEvents whichHand $ \case
            HandStateEvent hand -> do                
                let V2 x y  = (hand ^. hndXY & _y *~ (-1)) * keyboardDims
                    pose = V3 x 0.1 (y + keyOffsetY + keyboardDims ^. _y / 2 - (keyHeightT / 2))
                setPose (identity & translation .~ pose)
            _ -> return ()