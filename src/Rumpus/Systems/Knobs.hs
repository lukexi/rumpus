module Rumpus.Systems.Knobs where
import PreludeExtra
import RumpusLib
import Rumpus.Systems.Shared
import Rumpus.Systems.Drag
import Rumpus.Systems.Text
import Rumpus.Systems.Physics
import Rumpus.Systems.Attachment
import Rumpus.Systems.SceneWatcher
import Rumpus.Systems.Hands
import Rumpus.Systems.Controls
import qualified Data.HashMap.Strict as Map

type KnobName = String

data KnobDef = KnobDef
    { knbScale        :: !KnobScale
    , knbDefault01    :: !Float
    , knbAction       :: !(GLfloat -> EntityMonad ())
    , knbVal01ToValue :: !(Float -> Float)
    , knbValueToVal01 :: !(Float -> Float)
    }

-- TODO: Nicer to make Linear/Exponential a tag of a "Smooth" knob, but seemed
-- to feel more complicated as an API
-- (e.g. addKnob (Smooth Linear 0 10) 1)
data KnobScale = Linear Float Float
               | Exponential Float Float
               | Stepped [String]

data KnobState = KnobState
    { ksLastHandPose :: !(M44 GLfloat)
    , ksRotation     :: !GLfloat
    }
newKnobState :: KnobState
newKnobState = KnobState identity 0


type KnobDefs   = Map KnobName KnobDef
type KnobValues = Map KnobName Float
type KnobLayoutScale = Float

defineComponentKey ''KnobDefs
defineComponentKey ''KnobValues
defineComponentKey ''KnobState
defineComponentKey ''KnobLayoutScale

type KnobValueCached = Float
type KnobValueCachedNew = Bool
defineComponentKey ''KnobValueCached
defineComponentKey ''KnobValueCachedNew

initKnobsSystem :: MonadState ECS m => m ()
initKnobsSystem = do
    registerComponent "KnobDefs"           myKnobDefs           (newComponentInterface myKnobDefs)
    registerComponent "KnobState"          myKnobState          (newComponentInterface myKnobState)
    registerComponent "KnobLayoutScale"    myKnobLayoutScale    (newComponentInterface myKnobLayoutScale)
    registerComponent "KnobValues"         myKnobValues         (savedComponentInterface myKnobValues)

    registerComponent "KnobValueCached"    myKnobValueCached    (newComponentInterface myKnobValueCached)
    registerComponent "KnobValueCachedNew" myKnobValueCachedNew (newComponentInterface myKnobValueCachedNew)

addKnobDef :: (MonadState ECS m, MonadReader EntityID m)
           => KnobName -> KnobDef -> m ()
addKnobDef knobName knobDef = prependComponent myKnobDefs (Map.singleton knobName knobDef)

-- Must use prependComponent rather than appendComponent to update the Map,
-- as its <> is left-biased
setKnobValue01 :: (MonadState ECS m, MonadReader EntityID m) => KnobName -> Float -> m ()
setKnobValue01 knobName value = prependComponent myKnobValues (Map.singleton knobName value)

getKnobValue01ByName :: (MonadState ECS m, MonadReader EntityID m) => KnobName -> m Float
getKnobValue01ByName knobName = do
    knobValues <- getComponentDefault mempty myKnobValues
    case Map.lookup knobName knobValues of
        Just value -> return value
        Nothing -> do
            knobDefs <- getComponentDefault mempty myKnobDefs
            case Map.lookup knobName knobDefs of
                Just knobDef -> return (knbDefault01 knobDef)
                Nothing      -> return 0

getEntityKnobValue01ByName :: (MonadState ECS m) => EntityID -> KnobName -> m Float
getEntityKnobValue01ByName entityID knobName = inEntity entityID (getKnobValue01ByName knobName)

getEntityKnobValueByName :: (MonadState ECS m) => EntityID -> KnobName -> m Float
getEntityKnobValueByName entityID knobName = inEntity entityID (getKnobValueByName knobName)

getKnobLayoutScale :: (MonadState ECS m, MonadReader EntityID m) => m Float
getKnobLayoutScale = getComponentDefault 0.5 myKnobLayoutScale

getEntityKnobLayoutScale :: (MonadState ECS m) => EntityID -> m Float
getEntityKnobLayoutScale entityID = inEntity entityID getKnobLayoutScale

getKnobValue :: (MonadState ECS m) => EntityID -> m Float
getKnobValue knobID = getEntityComponentDefault 0 knobID myKnobValueCached

-- For cases where we only want to do something semi-expensive on knob changes
getNewKnobValue :: (MonadState ECS m) => EntityID -> m (Maybe Float)
getNewKnobValue knobID = do
    isNew <- getEntityComponentDefault False knobID myKnobValueCachedNew
    if isNew
        then do
            inEntity knobID (myKnobValueCachedNew ==> False)
            Just <$> getEntityComponentDefault 0 knobID myKnobValueCached
        else return Nothing

whenNewKnobValue :: MonadState ECS m => EntityID -> (Float -> m a) -> m ()
whenNewKnobValue knobID action = do
    mNewValue <- getNewKnobValue knobID
    forM_ mNewValue action

-- Faster to use getKnobValue on the knob entity itself
getKnobValueByName :: (MonadState ECS m, MonadReader EntityID m) => KnobName -> m Float
getKnobValueByName knobName = do
    knobDefs <- getComponentDefault mempty myKnobDefs
    case Map.lookup knobName knobDefs of
        Just knobDef -> do
            knbVal01ToValue knobDef <$> getKnobValue01ByName knobName
        Nothing -> return 0

addKnob :: KnobName -> KnobScale -> Float -> EntityMonad EntityID
addKnob name knobScale defVal = addActiveKnob name knobScale defVal (const (return ()))

addActiveKnob :: KnobName
              -> KnobScale
              -> Float
              -> (GLfloat -> EntityMonad ())
              -> EntityMonad EntityID
addActiveKnob name knobScale defVal action = do
    layoutScale <- getKnobLayoutScale

    let knobsPerRow = 4

    i <- Map.size <$> getComponentDefault mempty myKnobDefs
    let xI = fromIntegral (i `div` knobsPerRow)
        yI = negate $
            fromIntegral (i `mod` knobsPerRow) - fromIntegral knobsPerRow / 2
    let x = xI * 0.4 * layoutScale
        y = yI * 0.3 * layoutScale
        knobPos = V3 (x + 0.5) y 0

    addActiveKnobAt layoutScale knobPos name
        (makeKnobDef knobScale defVal action)

makeKnobDef :: KnobScale -> Float -> (GLfloat -> EntityMonad ()) -> KnobDef
makeKnobDef knobScale defVal action =
    let (low, high) = case knobScale of
            Linear      l h -> (l, h)
            Exponential l h -> (l, h)
            Stepped options -> (0, fromIntegral (length options) - 1)
        range = high - low
        val01ToValue value01 = case knobScale of
            Linear      _ _ -> low + range * value01
            Exponential _ _ -> low + range * (value01 ^ (2::Int))
            -- E.g. for [foo,bar,baz] 0-0.33 should be 0,
            -- 0.33-0.66 should be 1, 0.66-1 should be 2
            Stepped _       -> fromIntegral . (\i -> i::Int) . floor
                                . min (range + 1 - 0.001)
                                $ (range + 1) * value01
        valueToVal01 value = (value - low) / range

    in KnobDef
        { knbScale        = knobScale
        , knbDefault01    = valueToVal01 defVal
        , knbAction       = action
        , knbVal01ToValue = val01ToValue
        , knbValueToVal01 = valueToVal01
        }

addActiveKnobAt :: GLfloat -> V3 GLfloat -> KnobName -> KnobDef -> EntityMonad EntityID
addActiveKnobAt knobLayoutScale knobPos name knobDef@KnobDef{..} = do
    let finalKnobLayoutScale = 0.1 * realToFrac knobLayoutScale
    addKnobDef name knobDef

    let initialRotation = value01ToKnobRotation knbDefault01


    initialValue01 <- getKnobValue01ByName name
    let initialValue = knbVal01ToValue initialValue01



    knbAction initialValue

    -- Position the text items relatively without inheriting the spin
    -- of the knob itself
    textContainer <- spawnChild $ do
        myPose ==> position knobPos
        mySize ==> finalKnobLayoutScale

    nameLabel <- spawnEntity $ do
        myText          ==> name
        myTextPose      ==> position (V3 0 1 0) !*! scaleMatrix 0.5
        myTransformType ==> RelativeFull
    valueLabel <- spawnEntity $ do
        myText          ==> displayValue knobDef initialValue
        myTextPose      ==> position (V3 0 -1 0) !*! scaleMatrix 0.5
        myTransformType ==> RelativeFull
    inEntity nameLabel  (setParent textContainer)
    inEntity valueLabel (setParent textContainer)

    -- These is added as a child of the knob after it is created
    knobLight <- spawnEntity $ do
        myShape         ==> Cube
        mySize          ==> V3 0.9 0.9 1.1
        myTransformType ==> RelativeFull
    let updateKnobLight value01 = do
            inEntity knobLight $ do
                setSize (V3 (0.9 * value01) (0.9 * value01) 1.1)
                setColor $ colorHSL value01 0.5 0.5
    updateKnobLight initialValue01

    parentID <- ask
    knob <- spawnChild $ do
        myShape        ==> Cube
        mySize         ==> finalKnobLayoutScale
        myBody         ==> Animated
        myDragOverride ==> True
        myDragBegan ==> do
            withComponent_ myActiveDrag $ \(ActiveDrag _handEntityID startM44) -> do
                myKnobState ==% \k -> k { ksLastHandPose = startM44 }
        myDragContinues ==> \_dragM44 -> do
            withComponent_ myActiveDrag $ \(ActiveDrag handEntityID _startM44) -> do
                -- Calculate the rotation for this tick
                newHandPose <- getEntityPose handEntityID
                oldState    <- getComponentDefault newKnobState myKnobState
                let diff = newHandPose `subtractMatrix` ksLastHandPose oldState
                    -- Eliminate below epsilon to avoid drift
                    V3 dX _dY _dZ = testEpsilon (quatToEuler (quaternionFromMatrix diff))
                    -- We want clockwise rotation, so bound to -2pi <> 0
                    newRotation = min 0 . max maxKnobRot $ ksRotation oldState + dX

                -- Haptic feedback based on turn rate
                when (abs dX > 0) $ do
                    mWhichHand <-  getWhichHand handEntityID
                    forM_ mWhichHand $ \whichHand -> do
                        hapticPulse whichHand (floor $ abs dX * 10000)

                -- Update the knob's state and appearance
                myKnobState ==> (KnobState { ksLastHandPose = newHandPose, ksRotation = newRotation })
                setAttachmentOffset (positionRotation knobPos
                    (axisAngle (V3 0 0 1) newRotation) )

                -- Update the knob's label
                let newValue01     = knobRotationToValue01 newRotation
                    newValue       = knbVal01ToValue newValue01
                inEntity valueLabel $ setText (displayValue knobDef newValue)
                -- Update the knob's "light"
                updateKnobLight newValue01

                -- Run the action
                knbAction newValue

                -- Record the knob value in the parent so it can be persisted
                inEntity parentID $ do
                    setKnobValue01 name newValue01
                -- Cache the knob value in the knob entity itself so it can be grabbed quickly
                myKnobValueCached    ==> newValue
                myKnobValueCachedNew ==> True
        myDragEnded ==> do
            sceneWatcherSaveEntity parentID
        myKnobState ==> newKnobState { ksRotation = initialRotation }
        myKnobValueCached    ==> initialValue
        myKnobValueCachedNew ==> True

    inEntity knobLight  (setParent knob)

    attachEntity knob
        (positionRotation knobPos
            (axisAngle (V3 0 0 1) initialRotation))
    return knob

knobRotationToValue01 :: Float -> Float
knobRotationToValue01 rot = rot / maxKnobRot
value01ToKnobRotation :: Float -> Float
value01ToKnobRotation val = val * maxKnobRot
twoPi :: Float
twoPi = 2 * pi
maxKnobRot :: Float
maxKnobRot = 3/4 * (-twoPi)

testEpsilon :: Epsilon a => a -> a
testEpsilon n = if nearZero n then 0 else n

displayValue :: KnobDef -> Float -> String
displayValue knobDef value = case knbScale knobDef of
    Linear _ _      -> (printf "%.2f" (value::Float))
    Exponential _ _ -> (printf "%.2f" (value::Float))
    Stepped options -> let i = round value in if i >= 0 && i < length options then options !! i else "<over>"
