{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Rumpus.Systems.KeyboardHands where
import PreludeExtra

import Rumpus.Systems.Controls
import Rumpus.Systems.Hands
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics
import Rumpus.Systems.Collisions
import Rumpus.Systems.Text
import Rumpus.Systems.Animation


import qualified Graphics.UI.GLFW.Pal as GLFW
import qualified Data.HashMap.Strict as Map

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
    , HandKeyLeft : cs "67890-="   "^&*()_+"  ++ [HandKeyBackspace,           HandKeyRight]
    , HandKeyLeft : cs "yuiop[]\\" "YUIOP{}|" ++ [                            HandKeyRight]
    , HandKeyLeft : cs "hjkl;'"    "HJKL:\""  ++ [HandKeyEnter, HandKeyEnter, HandKeyRight]
    , HandKeyLeft : cs "nm,./"     "NM<>?"    ++ [HandKeyShift,               HandKeyRight]
    , replicate 4 (HandKeyChar ' ' ' ')
    , replicate 4 HandKeyDown
    ]
    where
        cs unshifted shifted = map (uncurry HandKeyChar) (zip unshifted shifted)

keyWidth, keyHeight, keyPad, keyHeightT, keyWidthT, keyDepth, keyboardOffsetY, keyboardOffsetZ :: GLfloat
keyWidth        = 0.05
keyHeight       = 0.05
keyPad          = 0.01
keyWidthT       = keyWidth + keyPad
keyHeightT      = keyHeight + keyPad

keyDimsT :: V2 GLfloat
keyDimsT        = V2 keyWidthT keyHeightT

keyDepth = 0.02

keyColorOn, keyColorOff :: V4 GLfloat
keyColorOn               = hslColor 0.2 0.8 0.8
keyColorOff              = hslColor 0.3 0.8 0.4

-- How far up the controllers the keyboard appears
keyboardOffsetY = -0.2
-- How far off the controllers the keyboard floats
keyboardOffsetZ = 0.1

data KeyboardHandsSystem = KeyboardHandsSystem 
    { _kbhShiftDown   :: Bool
    , _kbhKeyIDs      :: [(EntityID, HandKey)]
    , _kbhCurrentKey  :: Map WhichHand HandKey
    , _kbhLastKey     :: Map WhichHand HandKey
    , _kbhKeyboard    :: Map WhichHand EntityID
    } deriving Show
makeLenses      ''KeyboardHandsSystem
defineSystemKey ''KeyboardHandsSystem


showKeyboardHands :: (MonadIO m, MonadState ECS m) => m ()
showKeyboardHands = do
    keyboardIDs <- viewSystem sysKeyboardHands kbhKeyboard
    forM_ keyboardIDs $ \keyboardID -> runEntity keyboardID $ do
        animateSizeTo 1 0.2

hideKeyboardHands :: (MonadIO m, MonadState ECS m) => m ()
hideKeyboardHands = do
    keyboardIDs <- viewSystem sysKeyboardHands kbhKeyboard
    forM_ keyboardIDs $ \keyboardID -> runEntity keyboardID $ do
        animateSizeTo 0.01 0.2


startKeyboardHandsSystem :: ECSMonad ()
startKeyboardHandsSystem = do

    leftHandID  <- getLeftHandID
    rightHandID <- getRightHandID
    
    -- Have hands write their key events to this entityID
    -- so we can pass them along on click to the InternalEvents channel
    let handsWithKeys = [ (LeftHand,  leftHandID,  leftHandKeys)
                        , (RightHand, rightHandID, rightHandKeys)
                        ]
    containersAndKeyIDs <- forM handsWithKeys $ \(whichHand, handID, keyRows) -> do
        --runEntity handID removeChildren

        containerID <- spawnEntity $ do
            myParent                   ==> handID
            myInheritParentTransform   ==> InheritPose
        scaleContainerID <- spawnEntity $ do
            myParent                   ==> containerID
            myInheritParentTransform   ==> InheritFull

        keyIDsForHand <- spawnKeysForHand whichHand scaleContainerID keyRows
        return ((whichHand, containerID), keyIDsForHand)
    let keyIDs = concatMap snd containersAndKeyIDs
        keyboardIDs = Map.fromList (map fst containersAndKeyIDs)

    registerSystem sysKeyboardHands $ KeyboardHandsSystem
        { _kbhShiftDown   = False
        , _kbhKeyIDs      = keyIDs
        , _kbhCurrentKey  = mempty
        , _kbhLastKey     = mempty
        , _kbhKeyboard    = keyboardIDs
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
                            forM_ (keyToEvent isShiftDown currentKey) $ \event -> do
                                sendInternalEvent (GLFWEvent event)

            _ -> return ()

spawnKeysForHand :: (MonadIO m, MonadState ECS m) => WhichHand -> EntityID -> [[HandKey]] -> m [(EntityID, HandKey)]
spawnKeysForHand whichHand containerID keyRows = do
    let numRows      = fromIntegral (length keyRows)
        maxNumKeys   = fromIntegral $ maximum (map length keyRows)
        keyboardDims = V2 (maxNumKeys * keyWidthT) (numRows * keyHeightT)

    -- Add the indicator of thumb position
    void $ spawnEntity $ makeThumbNub whichHand containerID keyboardDims

    -- Spawn the keys and return their entityIDs
    fmap concat . forM (zip [0..] keyRows) $ \(indexY, keyRow) -> do
        let numKeys = fromIntegral (length keyRow)
        forM (zip [0..] keyRow) $ \(indexX, key) -> do
            keyID <- spawnEntity $ 
                makeKeyboardKey whichHand containerID indexX indexY numKeys numRows keyboardDims key
            return (keyID, key)


makeKeyboardKey :: (MonadState ECS m, MonadReader EntityID m) => WhichHand -> EntityID -> Int-> Int -> GLfloat -> GLfloat -> V2 GLfloat -> HandKey -> m ()
makeKeyboardKey whichHand containerID (fromIntegral -> x) (fromIntegral -> y) numKeys numRows keyboardDims key = do
    let pointIsInKey          = inRect (keyXY - keyDimsT/2) keyDimsT

        keyXY@(V2 keyX keyY)  = V2 (keyOffsetX + x * keyWidthT) (keyboardOffsetY + y * keyHeightT)
        keyOffsetX            = -keyWidthT * (numKeys - 1) / 2
        pose                  = V3 keyX keyboardOffsetZ keyY
        
        keyTitleScale         = 1 / (fromIntegral (length keyTitle))
        keyTitle              = showKey False key 
    myParent                 ==> containerID
    myText                   ==> keyTitle
    myTextPose               ==> mkTransformation 
                                      (axisAngle (V3 1 0 0) (-pi/2)) (V3 0 1 0) !*! scaleMatrix keyTitleScale
    myColor                  ==> keyColorOff
    myShapeType              ==> CubeShape
    myPhysicsProperties      ==> [NoPhysicsShape]
    myPose                   ==> (identity & translation .~ pose)
    mySize                   ==> V3 keyWidth keyDepth keyHeight
    myInheritParentTransform ==> InheritPose
    myUpdate ==> do
        withHandEvents whichHand $ \case
            HandStateEvent hand -> do
                let thumbXY  = thumbPosInKeyboard hand keyboardDims ^. _xz
                    isInKey  = pointIsInKey thumbXY
                    color    = if isInKey then keyColorOn else keyColorOff
                myColor ==> color

                when isInKey $ do
                    modifySystemState sysKeyboardHands $ kbhCurrentKey . at whichHand ?= key
            _ -> return ()

getThumbPos :: Hand -> V2 GLfloat
getThumbPos hand = hand ^. hndXY 
    & _y  *~ (-1) -- y is flipped 
    & _xy *~ 0.5  -- scale to -0.5 - 0.5

thumbPosInKeyboard :: Hand -> V2 GLfloat -> V3 GLfloat
thumbPosInKeyboard hand keyboardDims = V3 x keyboardOffsetZ offsetY
    where V2 x y  = getThumbPos hand * keyboardDims
          offsetY = y + keyboardOffsetY + keyboardDims ^. _y / 2 - (keyHeightT / 2)

-- | Create a ball that tracks the position of the thumb mapped to the position of the keys
makeThumbNub :: (MonadState ECS m, MonadReader EntityID m) => WhichHand -> EntityID -> V2 GLfloat -> m ()
makeThumbNub whichHand containerID keyboardDims = do
    
    myParent                 ==> containerID
    myColor                  ==> keyColorOn
    myShapeType              ==> SphereShape
    myPhysicsProperties      ==> [NoPhysicsShape]
    mySize                   ==> realToFrac keyDepth
    myInheritParentTransform ==> InheritPose
    myUpdate               ==> do
        withHandEvents whichHand $ \case
            HandStateEvent hand -> do                
                let pose = thumbPosInKeyboard hand keyboardDims
                setPose (identity & translation .~ pose)
            _ -> return ()

-- | Check if a point is in the given rectangle
inRect :: (Num a, Ord a) => V2 a -> V2 a -> V2 a -> Bool
inRect (V2 x y) (V2 w h) (V2 ptX ptY) =
    ptX > x && ptX < (x + w) && ptY > y && ptY < (y + h)