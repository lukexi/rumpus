{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Systems.KeyboardHands where
import PreludeExtra

import Rumpus.Systems.Controls
import Rumpus.Systems.Hands
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics
import Rumpus.Systems.Collisions
import Rumpus.Systems.Text

import qualified Graphics.UI.GLFW.Pal as GLFW

data HandKey = HandKeyChar Char Char
             | HandKeyEnter
             | HandKeyBackspace
             | HandKeyTab
             | HandKeyUp
             | HandKeyDown
             | HandKeyLeft
             | HandKeyRight
             | HandKeyShift
             | HandKeyBlank -- Just a spacer
             deriving (Eq, Show)


data HandKeyLayout = RegularKey
                   | FullWidthKey
                   | FullHeightKey

data KeyDef = KeyDef HandKeyLayout HandKey

showKey :: Bool -> HandKey -> [Char]
showKey False (HandKeyChar unshifted _) = [unshifted]
showKey True  (HandKeyChar _ shifted)   = [shifted]
showKey _ HandKeyEnter                  = "Enter"
showKey _ HandKeyShift                  = "Shift"
showKey _ HandKeyBackspace              = "Backspace"
showKey _ HandKeyTab                    = "Tab"
showKey _ HandKeyUp                     = "^"
showKey _ HandKeyDown                   = "v"
showKey _ HandKeyLeft                   = "<"
showKey _ HandKeyRight                  = ">"
showKey _ HandKeyBlank                  = ""

keyToEvent :: Bool -> HandKey -> Maybe Event
keyToEvent False (HandKeyChar unshifted _) = Just (Character unshifted)
keyToEvent True  (HandKeyChar _ shifted)   = Just (Character shifted)
keyToEvent _ HandKeyEnter                  = Just (toPressedKey Key'Enter)
keyToEvent _ HandKeyBackspace              = Just (toPressedKey Key'Backspace)
keyToEvent _ HandKeyTab                    = Just (toPressedKey Key'Tab)
keyToEvent _ HandKeyUp                     = Just (toPressedKey Key'Up)
keyToEvent _ HandKeyDown                   = Just (toPressedKey Key'Down)
keyToEvent _ HandKeyLeft                   = Just (toPressedKey Key'Left)
keyToEvent _ HandKeyRight                  = Just (toPressedKey Key'Right)
keyToEvent _ _ = Nothing

toPressedKey :: Key -> Event
toPressedKey key = Key key noKeyCode KeyState'Pressed noModifierKeys
    where
        -- (FIXME: we don't use keycodes anywhere, remove from API for now)
        noKeyCode = 0
        noModifierKeys = GLFW.ModifierKeys False False False False


leftHandKeys :: [[HandKey]]
leftHandKeys =
    [ replicate 4 HandKeyUp                                      
    , HandKeyLeft :                cs "`12345" "~!@#$%" ++ [HandKeyRight] 
    , HandKeyLeft : HandKeyTab   : cs "qwert"  "QWERT"  ++ [HandKeyRight] 
    , HandKeyLeft : HandKeyBlank : cs "asdfg"  "ASDFG"  ++ [HandKeyRight]
    , HandKeyLeft : HandKeyShift : cs "zxcvb"  "ZXCVB"  ++ [HandKeyRight]
    , replicate 4 (HandKeyChar ' ' ' ')
    , replicate 4 HandKeyDown 
    ]
    where
        cs unshifted shifted = map (uncurry HandKeyChar) (zip unshifted shifted)

rightHandKeys :: [[HandKey]]
rightHandKeys =
    [ replicate 4 HandKeyUp
    , HandKeyLeft : cs "67890-="   "^&*()_+"  ++ [HandKeyBackspace, HandKeyRight]
    , HandKeyLeft : cs "yuiop[]\\" "YUIOP{}|" ++ [                  HandKeyRight]
    , HandKeyLeft : cs "hjkl;'"    "HJKL:\""  ++ [HandKeyEnter, HandKeyEnter, HandKeyRight]
    , HandKeyLeft : cs "nm,./"     "NM<>?"    ++ [HandKeyShift,     HandKeyRight]
    , replicate 4 (HandKeyChar ' ' ' ')
    , replicate 4 HandKeyDown
    ]
    where
        cs unshifted shifted = map (uncurry HandKeyChar) (zip unshifted shifted)

keyWidth, keyHeight, keyPad, keyHeightT, keyWidthT, keyboardOffsetY :: GLfloat
keyWidth        = 0.05
keyHeight       = 0.05
keyPad          = 0.01
keyWidthT       = keyWidth + keyPad
keyHeightT      = keyHeight + keyPad

-- How far up the controllers the keyboard appears
keyboardOffsetY = -0.2


data KeyboardHandsSystem = KeyboardHandsSystem 
    { _kbhShiftDown   :: Bool
    , _kbhKeyIDs      :: [(EntityID, HandKey)]
    , _kbhCurrentKey  :: Map WhichHand HandKey
    , _kbhLastKey     :: Map WhichHand HandKey
    } deriving Show
makeLenses      ''KeyboardHandsSystem
defineSystemKey ''KeyboardHandsSystem


startKeyboardHandsSystem :: ECSMonad ()
startKeyboardHandsSystem = do

    leftHandID  <- getLeftHandID
    rightHandID <- getRightHandID
    
    runEntity leftHandID $ do
        myOnCollisionStart  ==> \_ impulse -> do
            hapticPulse LeftHand (floor $ impulse * 10000)


    runEntity rightHandID $ do
        myOnCollisionStart  ==> \_ impulse -> 
            hapticPulse RightHand (floor $ impulse * 10000)
    
    -- Have hands write their key events to this entityID
    -- so we can pass them along on click to the InternalEvents channel
    let handsWithIDs = [ (LeftHand, leftHandID)
                       , (RightHand, rightHandID)
                       ]
        handsWithKeys = zip handsWithIDs [leftHandKeys, rightHandKeys]
    keyIDs <- fmap concat . forM handsWithKeys $ \((whichHand, handID), keyRows) -> do
        runEntity handID removeChildren
        spawnKeysForHand whichHand handID keyRows

    registerSystem sysKeyboardHands $ KeyboardHandsSystem
        { _kbhShiftDown   = False
        , _kbhKeyIDs      = keyIDs
        , _kbhCurrentKey  = mempty
        , _kbhLastKey     = mempty
        }

tickKeyboardHandsSystem :: ECSMonad ()
tickKeyboardHandsSystem = do
    handIDs <- getHandIDs

    keyIDs <- viewSystem sysKeyboardHands kbhKeyIDs

    forM_ handIDs $ \(whichHand, _handID) -> do
        withHandEvents whichHand $ \case
            HandStateEvent _ -> do
                modifySystemState sysKeyboardHands $ do
                    currentKey <- use $ kbhCurrentKey . at whichHand
                    lastKey    <- use $ kbhLastKey    . at whichHand
                    when (currentKey /= lastKey) $ do
                        lift $ hapticPulse whichHand 2000
                    kbhLastKey . at whichHand .= currentKey
            HandButtonEvent HandButtonPad ButtonDown -> do
                mCurrentKey <- viewSystem sysKeyboardHands (kbhCurrentKey . at whichHand)
                forM_ mCurrentKey $ \currentKey -> do

                    -- Shift handling
                    if (currentKey == HandKeyShift) 
                        then do
                            isShiftDown <- modifySystemState sysKeyboardHands (kbhShiftDown <%= not)
                            -- Flip the text of all the keys to reflect the shifted state
                            forM_ keyIDs $ \(keyID, key) -> do
                                runEntity keyID $ setText (showKey isShiftDown key)
                        -- We don't send any events for Shift, just using it to toggle internal state.
                        else do
                            isShiftDown <- viewSystem sysKeyboardHands kbhShiftDown
                            --sendInternalEvent (GLFWEvent (Key pendingEventKey _ keyState modifierKeyBools))
                            forM_ (keyToEvent isShiftDown currentKey) $ \event -> do
                                sendInternalEvent (GLFWEvent event)

            _ -> return ()

spawnKeysForHand :: (MonadIO m, MonadState ECS m) => WhichHand -> EntityID -> [[HandKey]] -> m [(EntityID, HandKey)]
spawnKeysForHand whichHand handID keyRows = do
    let numRows = fromIntegral (length keyRows)
        maxNumKeys = fromIntegral $ maximum (map length keyRows)

    -- Add the indicator of thumb position
    void $ spawnEntity $ makeThumbNub whichHand handID maxNumKeys numRows

    -- Spawn the keys and return their entityIDs
    fmap concat . forM (zip [0..] keyRows) $ \(indexY, keyRow) -> do
        let numKeys = fromIntegral (length keyRow)
        forM (zip [0..] keyRow) $ \(indexX, key) -> do
            keyID <- spawnEntity $ 
                makeKeyboardKey whichHand handID indexX indexY numKeys numRows key
            return (keyID, key)

makeKeyboardKey :: (MonadState ECS m, MonadReader EntityID m) => WhichHand -> EntityID -> Int-> Int -> GLfloat -> GLfloat -> HandKey -> m ()
makeKeyboardKey whichHand parentHandID x y numKeys numRows key = do
    let (indexXF,  indexYF)   = (fromIntegral x                   , fromIntegral y)
        (keyProgX, keyProgY)  = (indexXF / numKeys                , indexYF / numRows) 
        (keyProgW, keyProgH)  = (1 / numKeys                      , 1       / numRows)
        (keyX, keyY)          = (keyOffsetX + indexXF * keyWidthT , keyboardOffsetY + indexYF * keyHeightT)
        pointIsInKey          = inRect keyProgX keyProgY keyProgW keyProgH
        keyOffsetX            = -keyWidthT * (numKeys - 1) / 2
        pose                  = V3 keyX 0.1 keyY
        colorOn               = hslColor 0.2 0.8 0.8
        colorOff              = hslColor 0.3 0.8 0.4
        keyTitleScale         = 1 / (fromIntegral (length keyTitle))
        keyTitle              = showKey False key 
    myText                   ==> keyTitle
    myTextPose               ==> mkTransformation 
                                      (axisAngle (V3 1 0 0) (-pi/2)) (V3 0 1 0) !*! scaleMatrix keyTitleScale
    myColor                  ==> colorOff
    myParent                 ==> parentHandID
    myShapeType              ==> CubeShape
    myPhysicsProperties      ==> [NoPhysicsShape]
    myPose                   ==> (identity & translation .~ pose)
    mySize                   ==> V3 keyWidth 0.02 keyHeight
    myInheritParentTransform ==> InheritPose
    myOnUpdate ==> do
        withHandEvents whichHand $ \case
            HandStateEvent hand -> do
                let thumbXY  = getThumbPos hand
                    isInKey  = pointIsInKey (thumbXY + 0.5) -- pointIsInKey expects values 0-1 rather than -0.5 - 0.5
                    color    = if isInKey then colorOn else colorOff
                myColor ==> color

                when isInKey $ do
                    modifySystemState sysKeyboardHands $ kbhCurrentKey . at whichHand ?= key
            _ -> return ()

getThumbPos :: Hand -> V2 GLfloat
getThumbPos hand = hand ^. hndXY 
    & _y  *~ (-1) -- y is flipped 
    & _xy *~ 0.5  -- scale to -0.5 - 0.5

-- | Check if a point is in the given rectangle
inRect :: (Num a, Ord a) => a -> a -> a -> a -> V2 a -> Bool
inRect x y w h (V2 ptX ptY) =
    ptX > x && ptX < x + w && ptY > y && ptY < y + h

-- | Create a ball that tracks the position of the thumb mapped to the position of the keys
makeThumbNub :: (HasComponents s, MonadState s m, MonadReader EntityID m) => WhichHand -> EntityID -> GLfloat -> GLfloat -> m ()
makeThumbNub whichHand parentHandID maxNumKeys numRows = do
    let keyboardDims = V2 (maxNumKeys * keyWidthT) (numRows * keyHeightT)
        colorOn = hslColor 0.2 0.8 0.8
    myColor                  ==> colorOn
    myParent                 ==> parentHandID
    myShapeType              ==> SphereShape
    myPhysicsProperties      ==> [NoPhysicsShape]
    mySize                   ==> 0.02
    myInheritParentTransform ==> InheritPose
    myOnUpdate               ==> do
        withHandEvents whichHand $ \case
            HandStateEvent hand -> do                
                let V2 x y  = getThumbPos hand * keyboardDims
                    pose    = V3 x 0.1 (y + keyboardOffsetY + keyboardDims ^. _y / 2 - (keyHeightT / 2))
                setPose (identity & translation .~ pose)
            _ -> return ()