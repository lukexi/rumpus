module Rumpus.Systems.KeyPads where
import PreludeExtra
import RumpusLib

import Rumpus.Systems.Controls
import Rumpus.Systems.Hands
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics
import Rumpus.Systems.Text
import Rumpus.Systems.Selection
import Rumpus.Systems.Animation
import Rumpus.Systems.Clock

import qualified Graphics.UI.GLFW.Pal as GLFW
import qualified Data.HashMap.Strict as Map

data HandKey = HandKeyChar Char Char
             | HandKeyEnter
             | HandKeyBackspace
             | HandKeyTab
             | HandKeyCut
             | HandKeyCopy
             | HandKeyPaste
             | HandKeyMoveLineUp
             | HandKeyMoveLineDown
             | HandKeyIndent
             | HandKeyUnIndent
             | HandKeyUp
             | HandKeyDown
             | HandKeyLeft
             | HandKeyRight
             | HandKeyShift
             | HandKeyBlank -- Just a spacer
             deriving (Eq, Show)

keyCapWidth :: HandKey -> GLfloat
keyCapWidth (HandKeyChar ' ' ' ') = 8
keyCapWidth (HandKeyChar _ _)     = 1
keyCapWidth HandKeyEnter          = 2
keyCapWidth HandKeyShift          = 2
keyCapWidth HandKeyBackspace      = 2
keyCapWidth HandKeyTab            = 2
keyCapWidth HandKeyCut            = 1.5
keyCapWidth HandKeyCopy           = 1.5
keyCapWidth HandKeyPaste          = 1.6
keyCapWidth HandKeyMoveLineUp     = 1.4
keyCapWidth HandKeyMoveLineDown   = 1.4
keyCapWidth HandKeyIndent         = 1.6
keyCapWidth HandKeyUnIndent       = 1.6
keyCapWidth HandKeyUp             = 8
keyCapWidth HandKeyDown           = 8
keyCapWidth HandKeyLeft           = 1
keyCapWidth HandKeyRight          = 1
keyCapWidth HandKeyBlank          = 2


keyCapHeight :: HandKey -> GLfloat
keyCapHeight HandKeyLeft          = 7
keyCapHeight HandKeyRight         = 7
keyCapHeight _                    = 1

showKey :: Bool -> HandKey -> [Char]
showKey False (HandKeyChar unshifted _) = [unshifted]
showKey True  (HandKeyChar _ shifted)   = [shifted]
showKey _ HandKeyEnter                  = "Enter"
showKey _ HandKeyShift                  = "Shift"
showKey _ HandKeyBackspace              = "<-"
showKey _ HandKeyTab                    = "Tab"
showKey _ HandKeyCut                    = "Cut"
showKey _ HandKeyCopy                   = "Copy"
showKey _ HandKeyPaste                  = "Paste"
showKey _ HandKeyMoveLineUp             = "L^^"
showKey _ HandKeyMoveLineDown           = "Lvv"
showKey _ HandKeyIndent                 = "L>>"
showKey _ HandKeyUnIndent               = "L<<"
showKey _ HandKeyUp                     = "^"
showKey _ HandKeyDown                   = "v"
showKey _ HandKeyLeft                   = "<"
showKey _ HandKeyRight                  = ">"
showKey _ HandKeyBlank                  = ""

keyToEvent :: Bool -> HandKey -> Maybe Event
--         shift                                                shift ctrl
keyToEvent shift HandKeyEnter              = Just (toPressedKey shift False Key'Enter)
keyToEvent shift HandKeyBackspace          = Just (toPressedKey shift False Key'Backspace)
keyToEvent shift HandKeyTab                = Just (toPressedKey shift False Key'Tab)
keyToEvent shift HandKeyUp                 = Just (toPressedKey shift False Key'Up)
keyToEvent shift HandKeyDown               = Just (toPressedKey shift False Key'Down)
keyToEvent shift HandKeyLeft               = Just (toPressedKey shift False Key'Left)
keyToEvent shift HandKeyRight              = Just (toPressedKey shift False Key'Right)
keyToEvent shift HandKeyIndent             = Just (toPressedKey shift True Key'RightBracket)
keyToEvent shift HandKeyUnIndent           = Just (toPressedKey shift True Key'LeftBracket)
keyToEvent _     HandKeyMoveLineUp         = Just (toPressedKey True  True Key'Up)
keyToEvent _     HandKeyMoveLineDown       = Just (toPressedKey True  True Key'Down)
keyToEvent _     HandKeyCut                = Just (toPressedKey False True Key'X)
keyToEvent _     HandKeyCopy               = Just (toPressedKey False True Key'C)
keyToEvent _     HandKeyPaste              = Just (toPressedKey False True Key'V)
keyToEvent False (HandKeyChar unshifted _) = Just (Character unshifted)
keyToEvent True  (HandKeyChar _ shifted)   = Just (Character shifted)
keyToEvent _ _ = Nothing

toPressedKey :: Bool -> Bool -> GLFW.Key -> Event
toPressedKey shift control key = KeyboardKey key noKeyCode KeyState'Pressed modifierKeys
    where
        -- (FIXME: we don't use keycodes anywhere, remove from API for now)
        noKeyCode = 0
        modifierKeys = GLFW.ModifierKeys shift control alt super
        (alt, super) = (False, False)


leftHandKeys :: [[HandKey]]
leftHandKeys =
    [ [HandKeyUp]
    , [HandKeyMoveLineUp, HandKeyMoveLineDown, HandKeyCut, HandKeyCopy, HandKeyPaste]
    ,                   cs "`123456" "~!@#$%^"
    , HandKeyIndent   : cs "qwert"  "QWERT"
    , HandKeyUnIndent : cs "asdfg"  "ASDFG"
    , HandKeyShift : cs "zxcvb"  "ZXCVB"
    , [HandKeyChar ' ' ' ']
    , [HandKeyDown]
    ]
    where
        cs unshifted shifted = map (uncurry HandKeyChar) (zip unshifted shifted)

rightHandKeys :: [[HandKey]]
rightHandKeys =
    [ [HandKeyUp]
    , [HandKeyMoveLineUp, HandKeyMoveLineDown, HandKeyCut, HandKeyCopy, HandKeyPaste]
    , cs "7890-="   "&*()_+"  ++ [HandKeyBackspace]
    , cs "yuiop[]\\" "YUIOP{}|"
    , cs "hjkl;'"    "HJKL:\""  ++ [HandKeyEnter]
    , cs "nm,./"     "NM<>?"    ++ [HandKeyShift]
    , [HandKeyChar ' ' ' ']
    , [HandKeyDown]
    ]
    where
        cs unshifted shifted = map (uncurry HandKeyChar) (zip unshifted shifted)

keyWidth, keyHeight, keyDepth, keyPadding :: GLfloat
keyWidth        = 0.05
keyHeight       = 0.05
keyDepth        = 0.02
keyPadding      = 0.01

keyHeightT, keyWidthT :: GLfloat
keyWidthT       = keyWidth + keyPadding
keyHeightT      = keyHeight + keyPadding

keyColorOn, keyColorOff :: V4 GLfloat
keyColorOn               = colorHSL 0.2 0.8 0.8
keyColorOff              = colorHSL 0.3 0.8 0.4

minimizedKeyPadSize :: V3 GLfloat
minimizedKeyPadSize = 0.001

maximizedKeyPadSize :: V3 GLfloat
maximizedKeyPadSize = 0.3

keyPadAnimDur :: DiffTime
keyPadAnimDur = 0.2

data KeyPad = KeyPad
    { _kpdKeyPadID    :: EntityID
    , _kpdKeys        :: [KeyPadKey]
    , _kpdThumbNub    :: EntityID
    , _kpdKeyRepeater :: Maybe EntityID
    , _kpdDims        :: (V2 Float)
    , _kpdShiftDown   :: Bool
    , _kpdCurrentKey  :: Maybe HandKey
    , _kpdLastKey     :: Maybe HandKey
    }
data KeyPadKey = KeyPadKey
    { _kpkKeyBackID    :: EntityID
    , _kpkKeyNameID    :: EntityID
    , _kpkKey          :: HandKey
    , _kpkPointIsInKey :: V2 GLfloat -> Bool
    }
makeLenses ''KeyPadKey
makeLenses ''KeyPad

--start :: Start
--start = void spawnKeyPads
spawnKeyPads :: ECSMonad (EntityID, [(WhichHand, KeyPad)])
spawnKeyPads = do

    -- Have hands write their key events to this entityID
    -- so we can pass them along on click to the InternalEvents channel
    let handsWithKeys = [ (LeftHand,  leftHandKeys,  V3 (-0.3) 0 0.1)
                        , (RightHand, rightHandKeys, V3   0.3  0 0.1)
                        ]
    --keyPadContainerID <- spawnChild (return ())
    keyPadContainerID <- spawnEntity (return ())
    keyPads <- forM handsWithKeys $ \(whichHand, keyRows, offset) -> do

        keyPadID         <- spawnEntity $ do
            myParent         ==> keyPadContainerID
            --mySize           ==> maximizedKeyPadSize
            mySize           ==> minimizedKeyPadSize
            myPose           ==> positionRotation
                                    offset
                                    (axisAngle (V3 1 0 0) (pi/2))
        scaleContainerID <- spawnEntity $ do
            myParent         ==> keyPadID
            myTransformType  ==> InheritFull


        (keyPadKeys, keyPadDims) <- spawnKeysForHand scaleContainerID keyRows

        -- Add the indicator of thumb position
        thumbNubID <- spawnEntity $ makeThumbNub scaleContainerID keyPadDims

        let V2 dimX dimY = keyPadDims
        inEntity scaleContainerID $ do
            setPosition (V3 (-dimX/2) 0 (-dimY/2))
            -- Visualize dimensions of keypad
            --spawnChild $ do
            --    myShape ==> Cube
            --    myPose ==> translateMatrix (V3 (keyPadDims^._x/2) 0 (keyPadDims^._y/2))
            --    mySize ==> V3 (keyPadDims^._x) 0.01 (keyPadDims^._y)



        let keyPad = KeyPad
                { _kpdKeyPadID    = keyPadID
                , _kpdKeys        = keyPadKeys
                , _kpdKeyRepeater = Nothing
                , _kpdThumbNub    = thumbNubID
                , _kpdDims        = keyPadDims
                , _kpdShiftDown   = False
                , _kpdCurrentKey  = Nothing
                , _kpdLastKey     = Nothing
                }
        return (whichHand, keyPad)
    --return ()
    return (keyPadContainerID, keyPads)

makeKeyPadKey :: (MonadIO m, MonadState ECS m)
              => EntityID -> HandKey -> GLfloat -> GLfloat -> m (KeyPadKey, V3 GLfloat)
makeKeyPadKey containerID key xOffset y = do
    let (keyPose, keySize) = getKeyPose key xOffset y
    (keyBackID, keyNameID) <- makeKeyboardKey containerID key keyPose keySize
    return $ (KeyPadKey
        { _kpkKeyBackID    = keyBackID
        , _kpkKeyNameID    = keyNameID
        , _kpkKey          = key
        , _kpkPointIsInKey = inRectWithCenter (keyPose^._xz) (keySize^._xz)
        }, keySize)

spawnKeysForHand :: (MonadIO m, MonadState ECS m)
                 => EntityID
                 -> [[HandKey]]
                 -> m ([KeyPadKey], V2 GLfloat)
spawnKeysForHand containerID keyRows = do

    let totalKeyRows = length keyRows
        arrowY = (fromIntegral totalKeyRows+1.7) * keyHeightT

    -- Spawn left arrow
    (leftKey, _) <- makeKeyPadKey containerID HandKeyLeft 0 arrowY


    -- Spawn keys
    keyPadKeysByRow <- forM (zip ([0..]::[Int]) keyRows) $ \(rowNum, keyRow) -> do
        foldM (\(xOffset, rowKeyPadKeysSoFar) key -> do
            (keyPadKey, keySize) <- makeKeyPadKey containerID key xOffset (fromIntegral rowNum)
            let newAccum = keyPadKey:rowKeyPadKeysSoFar

            return (xOffset+keySize^._x+keyPadding, newAccum)) (keyWidth+keyPadding,[]) keyRow

    -- Spawn right arrow
    let widestXOffset = maximum $ map fst keyPadKeysByRow
    (rightKey, rightKeySize) <- makeKeyPadKey containerID HandKeyRight widestXOffset arrowY

    let finalDimens = V2 (widestXOffset + rightKeySize ^. _x)
                         (fromIntegral totalKeyRows * keyHeight
                          + (fromIntegral totalKeyRows - 1) * keyPadding)
    return (leftKey : rightKey : concatMap snd keyPadKeysByRow, finalDimens)

getKeyPose :: HandKey -> GLfloat -> GLfloat
           -> (V3 GLfloat, V3 GLfloat)
getKeyPose key x r = (keyPose, keySize)
    where
        keyXY         = V2 (x + keySize^._x/2)
                           (r * (keySize^._z+keyPadding)+keyHeight/2)

        keyPose       = V3 (keyXY ^. _x) 0 (keyXY ^. _y)
        keySize       = V3 keyWidthFinal keyDepth keyHeightFinal

        keyWidthFinal  = keyWidth * widthMult
        keyHeightFinal = keyHeight * heightMult
        widthMult      = keyCapWidth key
        heightMult     = keyCapHeight key


makeKeyboardKey :: (MonadState ECS m, MonadIO m)
                => EntityID -> HandKey -> V3 GLfloat -> V3 GLfloat -> m (EntityID, EntityID)
makeKeyboardKey containerID key keyPosition keySize = do
    keyBackID <- spawnEntity $ do
        myParent                 ==> containerID
        myColor                  ==> keyColorOff
        myShape                  ==> Cube
        myPose                   ==> translateMatrix keyPosition
        mySize                   ==> keySize
    -- Spawn key name separately so it doesn't inherit the stretched size of its parent
    keyNameID <- spawnEntity $ do
        let keyTitleScale = if length keyTitle > 1
                then 1 / 2
                else 1
            keyTitle      = showKey False key
        myParent   ==> keyBackID
        myText     ==> keyTitle
        myTextPose ==> positionRotation
                          (V3 0 (keyDepth/2 + 0.001) 0)
                          (axisAngle (V3 1 0 0) (-pi/2))
                        !*! scaleMatrix (keyTitleScale * realToFrac keyWidth)

    return (keyBackID, keyNameID)


getThumbPos :: Hand -> V2 GLfloat
getThumbPos hand = hand ^. hndXY
    & _y  *~ (-1) -- y is flipped
    & _xy *~ 0.5  -- scale to -0.5 - 0.5

thumbPosInKeyboard :: Hand -> V2 GLfloat -> V3 GLfloat
thumbPosInKeyboard hand keyboardDims = V3 offsetX 0 offsetY
    where V2 x y  = getThumbPos hand * keyboardDims
          offsetY = y + keyboardDims ^. _y / 2
          offsetX = x + keyboardDims ^. _x / 2

-- | Create a ball that tracks the position of the thumb mapped to the position of the keys
makeThumbNub :: (MonadState ECS m, MonadReader EntityID m) => EntityID -> V2 GLfloat -> m ()
makeThumbNub containerID (V2 x y) = do

    myParent           ==> containerID
    myColor            ==> keyColorOn
    myShape            ==> Sphere
    mySize             ==> realToFrac keyDepth * 2
    myPose             ==> translateMatrix (V3 (x / 2) 0 (y / 2))

-- | Check if a point is in the given rectangle
inRect :: (Num a, Ord a) => V2 a -> V2 a -> V2 a -> Bool
inRect (V2 x y) (V2 w h) (V2 ptX ptY) =
    ptX > x && ptX < (x + w) && ptY > y && ptY < (y + h)

inRectWithCenter :: (Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> Bool
inRectWithCenter pose size = inRect topLeft size
    where topLeft = pose - size/2


-- System specific code

data KeyPadsSystem = KeyPadsSystem
    { _kpsKeyPads           :: Map WhichHand KeyPad
    , _kpsKeyPadContainer   :: EntityID
    }
defineSystemKey ''KeyPadsSystem
makeLenses ''KeyPadsSystem


startKeyPadsSystem :: ECSMonad ()
startKeyPadsSystem = do
    (keyPadContainerID, keyPads) <- spawnKeyPads
    registerSystem sysKeyPads $ KeyPadsSystem
        { _kpsKeyPads         = Map.fromList keyPads
        , _kpsKeyPadContainer = keyPadContainerID
        }


showKeyPads :: (MonadIO m, MonadState ECS m) => m ()
showKeyPads = do
    keyPadIDs <- viewSystemL sysKeyPads (kpsKeyPads . traverse . kpdKeyPadID)
    forM_ keyPadIDs $ \keyPadID -> inEntity keyPadID $ do
        setSize minimizedKeyPadSize
        animateSizeTo maximizedKeyPadSize keyPadAnimDur

hideKeyPads :: (MonadIO m, MonadState ECS m) => m ()
hideKeyPads = do
    keyPadIDs <- viewSystemL sysKeyPads (kpsKeyPads . traverse . kpdKeyPadID)
    forM_ keyPadIDs $ \keyPadID -> inEntity keyPadID $ do
        animateSizeTo minimizedKeyPadSize keyPadAnimDur

getKeyPadContainerID :: MonadState ECS m => m EntityID
getKeyPadContainerID = viewSystem sysKeyPads kpsKeyPadContainer



getThumbNub :: MonadState ECS m => WhichHand -> m (Maybe EntityID)
getThumbNub whichHand = viewSystemP sysKeyPads (kpsKeyPads . ix whichHand . kpdThumbNub)

getKeysForHand :: MonadState ECS m => WhichHand -> m [KeyPadKey]
getKeysForHand whichHand = viewSystemL sysKeyPads (kpsKeyPads . ix whichHand . kpdKeys . traverse)

getAllKeys :: MonadState ECS m => m [KeyPadKey]
getAllKeys = viewSystemL sysKeyPads (kpsKeyPads . traverse . kpdKeys . traverse)

tickKeyPadsSystem :: ECSMonad ()
tickKeyPadsSystem = do

    -- Sync the keys to the selected object manually to avoid interacting
    -- with the Child system (which the selected object might be using)
    -- This may become unnecessary with the proposed Deck system.
    traverseM_ getSelectedEntityID $ \selectedEntityID -> do
        isEditable <- entityHasComponent selectedEntityID myStartExpr
        when isEditable $ do
            keyPadContainerID <- getKeyPadContainerID
            selectedEntityPose <- getEntityPose selectedEntityID
            V3 _ _ sizeZ <- getEntitySize selectedEntityID
            inEntity keyPadContainerID $ setPose (selectedEntityPose !*! translateMatrix (V3 0 0 (sizeZ/2)))

    forM_ [LeftHand, RightHand] $ \whichHand -> do
        keysForHand <- getKeysForHand whichHand
        withHandEvents whichHand $ \case
            HandStateEvent hand -> do
                -- Update last/current keys, pulsing if changing
                modifySystemState sysKeyPads $ do
                    currentKey <- preuse $ kpsKeyPads . ix whichHand . kpdCurrentKey . traverse
                    lastKey    <- preuse $ kpsKeyPads . ix whichHand . kpdLastKey . traverse
                    when (currentKey /= lastKey) $ do
                        lift $ hapticPulse whichHand 1000
                    kpsKeyPads . ix whichHand . kpdLastKey .= currentKey

                keyboardDims <- fromMaybe (V2 0 0) <$> viewSystemP sysKeyPads (kpsKeyPads . ix whichHand . kpdDims)
                let thumbPos = thumbPosInKeyboard hand keyboardDims
                    thumbXY = thumbPos ^. _xz

                -- Update thumb nub
                traverseM_ (getThumbNub whichHand) $ \thumbNubID -> do
                    inEntity thumbNubID (setPosition thumbPos)

                -- Update active keys
                -- Default to no key, in case of thumb moving off keys all together.
                -- This will be overwritten below if a key is found.
                modifySystemState sysKeyPads $ kpsKeyPads . ix whichHand . kpdCurrentKey .= Nothing

                forM_ keysForHand $ \KeyPadKey{..} -> do

                    let isInKey  = _kpkPointIsInKey thumbXY
                        color    = if isInKey then keyColorOn else keyColorOff

                    inEntity _kpkKeyBackID (myColor ==> color)
                    when isInKey $ do
                        modifySystemState sysKeyPads $ kpsKeyPads . ix whichHand . kpdCurrentKey ?= _kpkKey
            HandButtonEvent HandButtonPad ButtonDown -> do
                mCurrentKey <- viewSystemP sysKeyPads (kpsKeyPads . ix whichHand . kpdCurrentKey . traverse)
                forM_ mCurrentKey $ \currentKey -> do

                    -- Shift handling
                    if (currentKey == HandKeyShift)
                        then do

                            -- Momentary shift
                            modifySystemState sysKeyPads (kpsKeyPads . ix whichHand . kpdShiftDown .= True)

                            -- Flip the text of all the keys to reflect the shifted state
                            let shiftIsDown = True
                            allKeys <- getAllKeys
                            forM_ allKeys $ \KeyPadKey{..} -> do
                                inEntity _kpkKeyNameID $ setText (showKey shiftIsDown _kpkKey)


                        -- We don't send any events for Shift, just using it to toggle internal state.
                        else do
                            isShiftDown <- or <$> viewSystemL sysKeyPads (kpsKeyPads . traverse . kpdShiftDown)
                            forM_ (keyToEvent isShiftDown currentKey) $ \event -> do
                                sendInternalEvent (GLFWEvent event)

                                -- Add a repeating key action
                                repeaterID <- spawnEntity $ return ()
                                inEntity repeaterID $
                                    setDelayedAction 0.25 $ do
                                        setRepeatingAction 0.025 $ do
                                            sendInternalEvent (GLFWEvent event)
                                modifySystemState sysKeyPads $
                                    kpsKeyPads . ix whichHand . kpdKeyRepeater ?= repeaterID

            HandButtonEvent HandButtonPad ButtonUp -> do
                -- Stop key-repeating
                mKeyRepeaterID <- viewSystemP sysKeyPads (kpsKeyPads . ix whichHand . kpdKeyRepeater . traverse)

                forM_ mKeyRepeaterID $ \keyRepeaterID -> do
                    removeEntity keyRepeaterID
                modifySystemState sysKeyPads $
                    kpsKeyPads . ix whichHand . kpdKeyRepeater .= Nothing

                -- Stop shifting when both shifts are off
                wasShiftDown <- or <$> viewSystemL sysKeyPads (kpsKeyPads . traverse . kpdShiftDown)
                modifySystemState sysKeyPads (kpsKeyPads . ix whichHand . kpdShiftDown .= False)
                isShiftDown <- or <$> viewSystemL sysKeyPads (kpsKeyPads . traverse . kpdShiftDown)
                when (wasShiftDown && not isShiftDown) $ do
                    allKeys <- getAllKeys
                    forM_ allKeys $ \KeyPadKey{..} -> do
                        inEntity _kpkKeyNameID $ setText (showKey False _kpkKey)

                    -- Allow iOS-style shift-drag-release to type chars
                    mCurrentKey <- viewSystemP sysKeyPads (kpsKeyPads . ix whichHand . kpdCurrentKey . traverse)
                    forM_ mCurrentKey $ \currentKey ->
                        forM_ (keyToEvent True currentKey) $ \event -> do
                            sendInternalEvent (GLFWEvent event)

            _ -> return ()


clearSelection :: (MonadIO m, MonadState ECS m) => m ()
clearSelection = do
    hideKeyPads
    clearSelectedEntityID


selectEntity :: (MonadIO m, MonadState ECS m) => EntityID -> m ()
selectEntity entityID = do
    getSelectedEntityID >>= \case
        Just prevSelectedID
            | prevSelectedID == entityID -> return ()
        _ -> do
            clearSelection

            setSelectedEntityID entityID

            isEditable <- entityHasComponent entityID myStartExpr
            when isEditable showKeyPads
