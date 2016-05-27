module KeyPads where
import PreludeExtra

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
keyCapWidth  (HandKeyChar ' ' ' ') = 7
keyCapWidth  (HandKeyChar _ _)     = 1
keyCapWidth  HandKeyEnter          = 2
keyCapWidth  HandKeyShift          = 2
keyCapWidth  HandKeyBackspace      = 2
keyCapWidth  HandKeyTab            = 2
keyCapWidth  HandKeyCut            = 2
keyCapWidth  HandKeyCopy           = 2
keyCapWidth  HandKeyPaste          = 2
keyCapWidth  HandKeyMoveLineUp     = 2
keyCapWidth  HandKeyMoveLineDown   = 2
keyCapWidth  HandKeyIndent         = 2
keyCapWidth  HandKeyUnIndent       = 2
keyCapWidth  HandKeyUp             = 7
keyCapWidth  HandKeyDown           = 7
keyCapWidth  HandKeyLeft           = 1
keyCapWidth  HandKeyRight          = 1
keyCapWidth  HandKeyBlank          = 2


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
    , [HandKeyCut, HandKeyCopy, HandKeyPaste]
    , [HandKeyIndent, HandKeyUnIndent, HandKeyMoveLineUp, HandKeyMoveLineDown]
    ,                cs "`12345" "~!@#$%"
    , HandKeyTab   : cs "qwert"  "QWERT"
    , HandKeyBlank : cs "asdfg"  "ASDFG"
    , HandKeyShift : cs "zxcvb"  "ZXCVB"
    , [HandKeyChar ' ' ' ']
    , [HandKeyDown]
    ]
    where
        cs unshifted shifted = map (uncurry HandKeyChar) (zip unshifted shifted)

rightHandKeys :: [[HandKey]]
rightHandKeys =
    [ [HandKeyUp]
    , [HandKeyCut, HandKeyCopy, HandKeyPaste]
    , [HandKeyIndent, HandKeyUnIndent, HandKeyMoveLineUp, HandKeyMoveLineDown]
    , cs "67890-="   "^&*()_+"  ++ [HandKeyBackspace]
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
    { _kpkKeyID        :: EntityID
    , _kpkKey          :: HandKey
    , _kpkPointIsInKey :: V2 GLfloat -> Bool
    }
makeLenses ''KeyPadKey
makeLenses ''KeyPad

start :: Start
start = do
    setSize (V3 0.4 0.4 0.1)

    -- Have hands write their key events to this entityID
    -- so we can pass them along on click to the InternalEvents channel
    let handsWithKeys = [ (LeftHand,  leftHandKeys,  V3 (-0.2) (0.25) 0.1)
                        , (RightHand, rightHandKeys, V3   0.2  (0.25) 0.1)
                        ]
    keyPadContainerID <- spawnChild $ do
        myInheritTransform ==> InheritPose
        return ()
    keyPads <- forM handsWithKeys $ \(whichHand, keyRows, offset) -> do

        keyPadID         <- spawnEntity $ do
            myParent             ==> keyPadContainerID
            myInheritTransform   ==> InheritPose
            mySize               ==> 0.3
            myPose               ==> mkTransformation
                                        (axisAngle (V3 1 0 0) (pi/2))
                                        offset
        scaleContainerID <- spawnEntity $ do
            myParent             ==> keyPadID
            myInheritTransform   ==> InheritFull

        -- Add the indicator of thumb position
        thumbNubID <- spawnEntity $ makeThumbNub scaleContainerID

        keyPadKeys <- spawnKeysForHand scaleContainerID keyRows

        let numRows    = fromIntegral (length keyRows)
            maxNumKeys = fromIntegral $ maximum (map length keyRows)
            keyPadDims = V2
                (maxNumKeys * keyWidthT)
                (numRows    * keyHeightT)
            keyPad = KeyPad
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
    return ()

spawnKeysForHand :: (MonadIO m, MonadState ECS m)
                 => EntityID
                 -> [[HandKey]]
                 -> m [KeyPadKey]
spawnKeysForHand containerID keyRows = do

    fmap concat . forM (zip [0..] keyRows) $ \(y, keyRow) -> do
        let numKeys = fromIntegral (length keyRow)
            rowXCentering = -keyWidthT * (numKeys - 1) / 2
        snd <$> foldM (\(x,accum) key -> do
            let (keyPose, keySize) = getKeyPose key x y rowXCentering


            keyID <- spawnEntity $ makeKeyboardKey containerID key keyPose keySize

            let newAccum = accum ++
                    [KeyPadKey
                        { _kpkKeyID        = keyID
                        , _kpkKey          = key
                        , _kpkPointIsInKey = inRectWithCenter (keyPose^._xz) (keySize^._xz)
                        }]
            return (x+keySize^._x+keyPadding, newAccum)) (0,[]) keyRow

inRectWithCenter pose size = inRect topLeft size
    where topLeft = pose - size/2
getKeyPose :: HandKey -> GLfloat -> Int -> GLfloat
           -> (V3 GLfloat, V3 GLfloat)
getKeyPose key x (fromIntegral -> y) rowXCentering = (keyPose, keySize)
    where
        keyDimsT      = keySize ^. _xz + V2 0 keyPadding

        keyXY         = V2 (x + keyDimsT ^. _x/2)
                           (y * keyDimsT ^. _y)

        keyPose       = V3 (keyXY ^. _x) 0 (keyXY ^. _y)
        keySize       = V3 keyWidthFinal keyDepth keyHeight

        keyWidthFinal = keyWidth * widthMult
                        -- For extra-wide keys, add in the padding width to fill gaps
                        + keyPadding * (widthMult - 1)

        widthMult     = keyCapWidth key





makeKeyboardKey :: (MonadState ECS m, MonadReader EntityID m)
                => EntityID -> HandKey -> V3 GLfloat -> V3 GLfloat -> m ()
makeKeyboardKey containerID key keyPosition keySize = do
    let keyTitleScale         = 1 / (fromIntegral (length keyTitle))
        keyTitle              = showKey False key
    myParent                 ==> containerID
    myText                   ==> keyTitle
    myTextPose               ==> mkTransformation
                                      (axisAngle (V3 1 0 0) (-pi/2))
                                      (V3 0 1 0)
                                    !*! scaleMatrix keyTitleScale
    myColor                  ==> keyColorOff
    myShape                  ==> Cube
    myProperties             ==> [Holographic]
    myPose                   ==> translateMatrix keyPosition
    mySize                   ==> keySize
    myInheritTransform       ==> InheritPose


getThumbPos :: Hand -> V2 GLfloat
getThumbPos hand = hand ^. hndXY
    & _y  *~ (-1) -- y is flipped
    & _xy *~ 0.5  -- scale to -0.5 - 0.5

thumbPosInKeyboard :: Hand -> V2 GLfloat -> V3 GLfloat
thumbPosInKeyboard hand keyboardDims = V3 x 0 offsetY
    where V2 x y  = getThumbPos hand * keyboardDims
          offsetY = y +
                    keyboardDims ^. _y / 2 - (keyHeightT / 2)

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
