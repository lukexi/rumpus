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
    { _kpsShiftDown         :: Map WhichHand Bool
    , _kpsKeyPadKeys        :: Map WhichHand [(EntityID, HandKey, V2 GLfloat -> Bool)]
    , _kpsCurrentKey        :: Map WhichHand HandKey
    , _kpsLastKey           :: Map WhichHand HandKey
    , _kpsKeyPad            :: Map WhichHand EntityID
    , _kpsKeyRepeaters      :: Map WhichHand EntityID
    , _kpsThumbNubs         :: Map WhichHand EntityID
    , _kpsKeyPadDims        :: Map WhichHand (V2 Float)
    , _kpsKeyPadContainer   :: EntityID
    }
makeLenses      ''KeyPadsSystem
defineSystemKey ''KeyPadsSystem


data KeyPad = KeyPad
    { _kpdKeyPadKeys        :: [KeyPadKey]
    , _kpdShiftDown         :: Bool
    , _kpdCurrentKey        :: HandKey
    , _kpdLastKey           :: HandKey
    , _kpdKeyPad            :: EntityID
    , _kpdKeyRepeater       :: EntityID
    , _kpdThumbNub          :: EntityID
    , _kpdKeyPadDims        :: (V2 Float)
    , _kpdKeyPadContainer   :: EntityID
    }
data KeyPadKey = KeyPadKey
    { _kpkKeyID        :: EntityID
    , _kpkKey          :: HandKey
    , _kpkPointIsInKey :: V2 GLfloat -> Bool
    }
makeLenses ''KeyPad
makeLenses ''KeyPadKey

showKeyPads :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
showKeyPads forEntityID = do
    keyPadContainerID <- viewSystem sysKeyPads kpsKeyPadContainer

    runEntity keyPadContainerID $ setParent forEntityID

    keyPadIDs <- viewSystem sysKeyPads kpsKeyPad
    forM_ keyPadIDs $ \keyPadID -> runEntity keyPadID $ do
        setSize 0.01
        animateSizeTo 0.3 0.2

hideKeyPads :: (MonadIO m, MonadState ECS m) => m ()
hideKeyPads = do
    keyPadIDs <- viewSystem sysKeyPads kpsKeyPad
    forM_ keyPadIDs $ \keyPadID -> runEntity keyPadID $ do
        animateSizeTo 0.01 0.2


startKeyPadsSystem :: ECSMonad ()
startKeyPadsSystem = do

    -- Have hands write their key events to this entityID
    -- so we can pass them along on click to the InternalEvents channel
    let handsWithKeys = [ (LeftHand,  leftHandKeys,  V3 (-0.2) (-0.25) 0.1)
                        , (RightHand, rightHandKeys, V3   0.2  (-0.25) 0.1)
                        ]
    keyPadContainerID <- spawnEntity $ do
        myInheritTransform ==> InheritPose
        return ()
    keyPads <- forM handsWithKeys $ \(whichHand, keyRows, offset) -> do

        keyPadID         <- spawnEntity $ do
            myParent             ==> keyPadContainerID
            myInheritTransform   ==> InheritPose
            mySize               ==> 0.01
            myPose               ==> mkTransformation (axisAngle (V3 1 0 0) (pi/2)) offset
        scaleContainerID <- spawnEntity $ do
            myParent             ==> keyPadID
            myInheritTransform   ==> InheritFull


        -- Add the indicator of thumb position
        thumbNubID <- spawnEntity $ makeThumbNub scaleContainerID

        keyPadKeys <- spawnKeysForHand scaleContainerID keyRows

        let numRows    = fromIntegral (length keyRows)
            maxNumKeys = fromIntegral $ maximum (map length keyRows)
            keyPadDims = V2 (maxNumKeys * keyWidthT) (numRows * keyHeightT)
        return (whichHand, keyPadID, thumbNubID, keyPadKeys, keyPadDims)

    let keyPadIDs   = Map.fromList $ zip (map (view _1) keyPads) (map (view _2) keyPads)
        thumbNubIDs = Map.fromList $ zip (map (view _1) keyPads) (map (view _3) keyPads)
        keyPadKeys  = Map.fromList $ zip (map (view _1) keyPads) (map (view _4) keyPads)
        keyPadDims  = Map.fromList $ zip (map (view _1) keyPads) (map (view _5) keyPads)

    registerSystem sysKeyPads $ KeyPadsSystem
        { _kpsShiftDown       = mempty
        , _kpsKeyPadKeys      = keyPadKeys
        , _kpsKeyPad          = keyPadIDs
        , _kpsCurrentKey      = mempty
        , _kpsLastKey         = mempty
        , _kpsKeyRepeaters    = mempty
        , _kpsKeyPadContainer = keyPadContainerID
        , _kpsThumbNubs       = thumbNubIDs
        , _kpsKeyPadDims      = keyPadDims
        }

getThumbNub :: MonadState ECS m => WhichHand -> m (Maybe EntityID)
getThumbNub whichHand = viewSystem sysKeyPads (kpsThumbNubs . at whichHand)
getKeysForHand :: MonadState ECS m => WhichHand -> m [(EntityID, HandKey, V2 GLfloat -> Bool)]
getKeysForHand whichHand = fromMaybe [] <$> viewSystem sysKeyPads (kpsKeyPadKeys . at whichHand)

tickKeyPadsSystem :: ECSMonad ()
tickKeyPadsSystem = do
    handIDs <- getHandIDs

    forM_ handIDs $ \(whichHand, _handID) -> do
        keysForHand <- getKeysForHand whichHand
        withHandEvents whichHand $ \case
            HandStateEvent hand -> do
                -- Update last/current keys, pulsing if changing
                modifySystemState sysKeyPads $ do
                    currentKey <- use $ kpsCurrentKey . at whichHand
                    lastKey    <- use $ kpsLastKey    . at whichHand
                    when (currentKey /= lastKey) $ do
                        lift $ hapticPulse whichHand 1000
                    kpsLastKey . at whichHand .= currentKey

                keyboardDims <- fromMaybe (V2 0 0) <$> viewSystem sysKeyPads (kpsKeyPadDims . at whichHand)
                let thumbPos = thumbPosInKeyboard hand keyboardDims
                    thumbXY = thumbPos ^. _xz

                -- Update thumb nub
                traverseM_ (getThumbNub whichHand) $ \thumbNubID -> do
                    runEntity thumbNubID (setPosition thumbPos)

                -- Update active keys
                -- Default to no key, in case of thumb moving off keys all together.
                -- This will be overwritten below if a key is found.
                modifySystemState sysKeyPads $ kpsCurrentKey . at whichHand .= Nothing

                forM_ keysForHand $ \(keyID, key, pointIsInKey) -> do

                    let isInKey  = pointIsInKey thumbXY
                        color    = if isInKey then keyColorOn else keyColorOff

                    runEntity keyID (myColor ==> color)
                    when isInKey $ do
                        modifySystemState sysKeyPads $ kpsCurrentKey . at whichHand ?= key
            HandButtonEvent HandButtonPad ButtonDown -> do
                mCurrentKey <- viewSystem sysKeyPads (kpsCurrentKey . at whichHand)
                forM_ mCurrentKey $ \currentKey -> do

                    -- Shift handling
                    if (currentKey == HandKeyShift)
                        then do

                            -- Momentary shift
                            modifySystemState sysKeyPads (kpsShiftDown . at whichHand ?= True)

                            -- Flip the text of all the keys to reflect the shifted state
                            let shiftIsDown = True
                            forM_ keysForHand $ \(keyID, key, _) -> do
                                runEntity keyID $ setText (showKey shiftIsDown key)


                        -- We don't send any events for Shift, just using it to toggle internal state.
                        else do
                            isShiftDown <- or . Map.elems <$> viewSystem sysKeyPads kpsShiftDown
                            forM_ (keyToEvent isShiftDown currentKey) $ \event -> do
                                sendInternalEvent (GLFWEvent event)

                                -- Add a repeating key action
                                repeaterID <- spawnEntity $ return ()
                                runEntity repeaterID $
                                    setDelayedAction 0.25 $ do
                                        setRepeatingAction 0.025 $ do
                                            sendInternalEvent (GLFWEvent event)
                                modifySystemState sysKeyPads $
                                    kpsKeyRepeaters . at whichHand ?= repeaterID

            HandButtonEvent HandButtonPad ButtonUp -> do
                -- Stop key-repeating
                mKeyRepeaterID <- viewSystem sysKeyPads (kpsKeyRepeaters . at whichHand)

                forM_ mKeyRepeaterID $ \keyRepeaterID -> do
                    removeEntity keyRepeaterID
                modifySystemState sysKeyPads $
                    kpsKeyRepeaters . at whichHand .= Nothing

                -- Stop shifting when both shifts are off
                wasShiftDown <- or . Map.elems <$> viewSystem sysKeyPads kpsShiftDown
                modifySystemState sysKeyPads (kpsShiftDown . at whichHand ?= False)
                isShiftDown <- or . Map.elems <$> viewSystem sysKeyPads kpsShiftDown
                when (wasShiftDown && not isShiftDown) $ do
                    forM_ keysForHand $ \(keyID, key, _) -> do
                        runEntity keyID $ setText (showKey False key)

            _ -> return ()

spawnKeysForHand :: (MonadIO m, MonadState ECS m)
                 => EntityID
                 -> [[HandKey]]
                 -> m [(EntityID, HandKey, V2 GLfloat -> Bool)]
spawnKeysForHand containerID keyRows = do

    -- Spawn the keys and return their entityIDs
    fmap concat . forM (zip [0..] keyRows) $ \(y, keyRow) -> do
        let numKeys = fromIntegral (length keyRow)
        forM (zip [0..] keyRow) $ \(x, key) -> do
            let (keyPose, pointIsInKey) = getKeyPose x y numKeys
            keyID <- spawnEntity $
                makeKeyboardKey containerID key keyPose
            return (keyID, key, pointIsInKey)

getKeyPose :: Int -> Int -> GLfloat -> (V3 GLfloat, V2 GLfloat -> Bool)
getKeyPose (fromIntegral -> x) (fromIntegral -> y) numKeys = (keyPose, pointIsInKey)
    where
        pointIsInKey          = inRect (keyXY - keyDimsT/2) keyDimsT

        keyXY@(V2 keyX keyY)  = V2 (keyOffsetX + x * keyWidthT) (keyboardOffsetY + y * keyHeightT)
        keyOffsetX            = -keyWidthT * (numKeys - 1) / 2
        keyPose               = V3 keyX keyboardOffsetZ keyY



makeKeyboardKey :: (MonadState ECS m, MonadReader EntityID m) => EntityID -> HandKey -> V3 GLfloat -> m ()
makeKeyboardKey containerID key keyPosition = do
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
makeThumbNub :: (MonadState ECS m, MonadReader EntityID m) => EntityID -> m ()
makeThumbNub containerID = do

    myParent           ==> containerID
    myColor            ==> keyColorOn
    myShape            ==> Sphere
    myProperties       ==> [Holographic]
    mySize             ==> realToFrac keyDepth * 2
    myInheritTransform ==> InheritPose

-- | Check if a point is in the given rectangle
inRect :: (Num a, Ord a) => V2 a -> V2 a -> V2 a -> Bool
inRect (V2 x y) (V2 w h) (V2 ptX ptY) =
    ptX > x && ptX < (x + w) && ptY > y && ptY < (y + h)
