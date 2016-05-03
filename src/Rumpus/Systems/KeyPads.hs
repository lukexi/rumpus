{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Rumpus.Systems.KeyPads where
import PreludeExtra

import Rumpus.Systems.Controls
import Rumpus.Systems.Hands
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics
import Rumpus.Systems.Text
import Rumpus.Systems.Animation
import Rumpus.Systems.Clock


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
toPressedKey key = KeyboardKey key noKeyCode KeyState'Pressed noModifierKeys
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

data KeyPadsSystem = KeyPadsSystem
    { _kbhShiftDown         :: Map WhichHand Bool
    , _kbhKeyPadKeys        :: Map WhichHand [(EntityID, HandKey, V2 GLfloat -> Bool)]
    , _kbhCurrentKey        :: Map WhichHand HandKey
    , _kbhLastKey           :: Map WhichHand HandKey
    , _kbhKeyPad            :: Map WhichHand EntityID
    , _kbhKeyRepeaters      :: Map WhichHand EntityID
    , _kbhKeyPadContainer   :: EntityID
    }
makeLenses      ''KeyPadsSystem
defineSystemKey ''KeyPadsSystem


showKeyPads :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
showKeyPads forEntityID = do
    keyboardID <- viewSystem sysKeyPads kbhKeyPadContainer

    runEntity keyboardID $ setParent forEntityID

    keyboardIDs <- viewSystem sysKeyPads kbhKeyPad
    forM_ keyboardIDs $ \keyboardID -> runEntity keyboardID $ do
        setSize 0.01
        animateSizeTo 0.3 0.2

hideKeyPads :: (MonadIO m, MonadState ECS m) => m ()
hideKeyPads = do
    keyboardIDs <- viewSystem sysKeyPads kbhKeyPad
    forM_ keyboardIDs $ \keyboardID -> runEntity keyboardID $ do
        animateSizeTo 0.01 0.2


startKeyPadsSystem :: ECSMonad ()
startKeyPadsSystem = do

    leftHandID  <- getLeftHandID
    rightHandID <- getRightHandID

    -- Have hands write their key events to this entityID
    -- so we can pass them along on click to the InternalEvents channel
    let handsWithKeys = [ (LeftHand,  leftHandID,  leftHandKeys,  V3 (-0.2) (-0.35) 0.1)
                        , (RightHand, rightHandID, rightHandKeys, V3   0.2  (-0.35) 0.1)
                        ]
    keyPadContainerID <- spawnEntity $ do
        myInheritTransform ==> InheritPose
        return ()
    containersAndKeyPadKeys <- forM handsWithKeys $ \(whichHand, handID, keyRows, offset) -> do

        keyPadID         <- spawnEntity $ do
            myParent             ==> keyPadContainerID
            myInheritTransform   ==> InheritPose
            mySize               ==> 0.01
            myPose               ==> mkTransformation (axisAngle (V3 1 0 0) (pi/2)) offset
        scaleContainerID <- spawnEntity $ do
            myParent             ==> keyPadID
            myInheritTransform   ==> InheritFull

        -- Add the indicator of thumb position
        thumbNubID <- spawnEntity $ makeThumbNub whichHand keyPadID

        keyPadKeys <- spawnKeysForHand whichHand scaleContainerID keyRows
        return (whichHand, keyPadID, thumbNubID, keyPadKeys)

    let keyPadIDs   = Map.fromList $ zip (view _1) (view _2) containersAndKeyPadKeys
        thumbNubIDs = Map.fromList $ zip (view _1) (view _3) containersAndKeyPadKeys
        keyPadKeys  = Map.fromList $ zip (view _1) (view _4) containersAndKeyPadKeys

    registerSystem sysKeyPads $ KeyPadsSystem
        { _kbhShiftDown       = mempty
        , _kbhKeyPadKeys      = keyPadKeys
        , _kbhCurrentKey      = mempty
        , _kbhLastKey         = mempty
        , _kbhKeyPad          = keyPadIDs
        , _kbhKeyRepeaters    = mempty
        , _kbhKeyPadContainer = keyPadContainerID
        , _kbhThumbNubs       = mempty
        }

tickKeyPadsSystem :: ECSMonad ()
tickKeyPadsSystem = do
    handIDs <- getHandIDs

    keyIDs <- viewSystem sysKeyPads kbhKeyPadKeys

    forM_ handIDs $ \(whichHand, _handID) -> do
        withHandEvents whichHand $ \case
            HandStateEvent _ -> do
                modifySystemState sysKeyPads $ do
                    currentKey <- use $ kbhCurrentKey . at whichHand
                    lastKey    <- use $ kbhLastKey    . at whichHand
                    when (currentKey /= lastKey) $ do
                        lift $ hapticPulse whichHand 1000
                    kbhLastKey . at whichHand .= currentKey
            HandButtonEvent HandButtonPad ButtonDown -> do
                mCurrentKey <- viewSystem sysKeyPads (kbhCurrentKey . at whichHand)
                forM_ mCurrentKey $ \currentKey -> do

                    -- Shift handling
                    if (currentKey == HandKeyShift)
                        then do

                            -- Momentary shift
                            modifySystemState sysKeyPads (kbhShiftDown . at whichHand ?= True)

                            -- Flip the text of all the keys to reflect the shifted state
                            let shiftIsDown = True
                            forM_ keyIDs $ \(keyID, key) -> do
                                runEntity keyID $ setText (showKey shiftIsDown key)


                        -- We don't send any events for Shift, just using it to toggle internal state.
                        else do
                            isShiftDown <- or . Map.elems <$> viewSystem sysKeyPads kbhShiftDown
                            forM_ (keyToEvent isShiftDown currentKey) $ \event -> do
                                sendInternalEvent (GLFWEvent event)

                                -- Add a repeating key action
                                repeaterID <- spawnEntity $ return ()
                                runEntity repeaterID $
                                    setDelayedAction 0.25 $ do
                                        setRepeatingAction 0.025 $ do
                                            sendInternalEvent (GLFWEvent event)
                                modifySystemState sysKeyPads $
                                    kbhKeyRepeaters . at whichHand ?= repeaterID

            HandButtonEvent HandButtonPad ButtonUp -> do
                -- Stop key-repeating
                mKeyRepeaterID <- viewSystem sysKeyPads (kbhKeyRepeaters . at whichHand)

                forM_ mKeyRepeaterID $ \keyRepeaterID -> do
                    removeEntity keyRepeaterID
                modifySystemState sysKeyPads $
                    kbhKeyRepeaters . at whichHand .= Nothing

                -- Stop shifting when both shifts are off
                wasShiftDown <- or . Map.elems <$> viewSystem sysKeyboardHands kbhShiftDown
                modifySystemState sysKeyboardHands (kbhShiftDown . at whichHand ?= False)
                isShiftDown <- or . Map.elems <$> viewSystem sysKeyboardHands kbhShiftDown
                when (wasShiftDown && not isShiftDown) $ do
                    forM_ keyIDs $ \(keyID, key) -> do
                        runEntity keyID $ setText (showKey False key)

            _ -> return ()

spawnKeysForHand :: (MonadIO m, MonadState ECS m) => WhichHand -> EntityID -> [[HandKey]] -> m [(EntityID, HandKey)]
spawnKeysForHand whichHand containerID keyRows = do
    let numRows      = fromIntegral (length keyRows)
        maxNumKeys   = fromIntegral $ maximum (map length keyRows)
        keyboardDims = V2 (maxNumKeys * keyWidthT) (numRows * keyHeightT)

    -- Spawn the keys and return their entityIDs
    fmap concat . forM (zip [0..] keyRows) $ \(y, keyRow) -> do
        let numKeys = fromIntegral (length keyRow)
        forM (zip [0..] keyRow) $ \(x, key) -> do
            let keyPose = getKeyPose x y numKeys keyboardDims
            keyID <- spawnEntity $
                makeKeyboardKey whichHand containerID key keyPose
            return (keyID, key)


getKeyPose (fromIntegral -> x) (fromIntegral -> y) keyboardDims numKeys = (keyPose, pointIsInKey)
    where
        pointIsInKey          = inRect (keyXY - keyDimsT/2) keyDimsT

        keyXY@(V2 keyX keyY)  = V2 (keyOffsetX + x * keyWidthT) (keyboardOffsetY + y * keyHeightT)
        keyOffsetX            = -keyWidthT * (numKeys - 1) / 2
        keyPose               = V3 keyX keyboardOffsetZ keyY

tickKeyPadsSystem = do
    forM_ [LeftHand, RightHand] $ \whichHand -> withHandEvents whichHand $ \case
        HandStateEvent hand -> do
            let thumbPos = thumbPosInKeyboard hand keyboardDims
                thumbXY = thumbPose ^. _xz
            -- Update thumb nub
            thumbNubID <- getThumbNub whichHand
            runEntity thumbNubID (setPosition thumbPos)

            -- Update active keys
            -- Default to no key, in case of thumb moving off keys all together.
            -- This will be overwritten below if a key is found.
            modifySystemState sysKeyPads $ kbhCurrentKey . at whichHand .= Nothing
            keysForHand <- getKeysForHand whichHand
            forM keysForHand $ \(keyID, key, pointIsInKey) -> do

                let isInKey  = pointIsInKey thumbXY
                    color    = if isInKey then keyColorOn else keyColorOff

                runEntity keyID (myColor ==> color)
                when isInKey $ do
                    modifySystemState sysKeyPads $ kbhCurrentKey . at whichHand ?= key
        _ -> return ()

makeKeyboardKey :: (MonadState ECS m, MonadReader EntityID m) => WhichHand -> EntityID -> HandKey -> V3 GLfloat -> m ()
makeKeyboardKey whichHand containerID key keyPosition = do
    let keyTitleScale         = 1 / (fromIntegral (length keyTitle))
        keyTitle              = showKey False key
    myParent                 ==> containerID
    myText                   ==> keyTitle
    myTextPose               ==> mkTransformation
                                      (axisAngle (V3 1 0 0) (-pi/2)) (V3 0 1 0)
                                    !*! scaleMatrix keyTitleScale
    myColor                  ==> keyColorOff
    myShape                  ==> Cube
    myProperties             ==> [Holographic]
    myPose                   ==> translateMatrix keyPosition
    mySize                   ==> V3 keyWidth keyDepth keyHeight
    myInheritTransform       ==> InheritPose


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

    myParent           ==> containerID
    myColor            ==> keyColorOn
    myShape            ==> Sphere
    myProperties       ==> [Holographic]
    mySize             ==> realToFrac keyDepth * 2
    myInheritTransform ==> InheritPose
    myUpdate           ==> do
        withHandEvents whichHand $ \case
            HandStateEvent hand -> do

            _ -> return ()

-- | Check if a point is in the given rectangle
inRect :: (Num a, Ord a) => V2 a -> V2 a -> V2 a -> Bool
inRect (V2 x y) (V2 w h) (V2 ptX ptY) =
    ptX > x && ptX < (x + w) && ptY > y && ptY < (y + h)
