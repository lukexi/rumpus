module Rumpus.Systems.Knobs where
import PreludeExtra
import RumpusLib
import Rumpus.Systems.Shared
import Rumpus.Systems.Drag
import Rumpus.Systems.Text
import Rumpus.Systems.Physics
import Rumpus.Systems.Script
import Rumpus.Systems.Attachment
import Rumpus.Systems.SceneWatcher
import qualified Data.HashMap.Strict as Map

type KnobName = String

data KnobDef = KnobDef
    { knbScale   :: !KnobScale
    , knbDefault :: !Float
    , knbAction  :: !(GLfloat -> EntityMonad ())
    }


data KnobScale = Linear Float Float
               | Stepped [String]

data KnobState = KnobState
    { ksLastHandPose :: !(M44 GLfloat)
    , ksRotation     :: !GLfloat
    }
newKnobState :: KnobState
newKnobState = KnobState identity 0


type KnobDefs = Map KnobName KnobDef
type KnobValues = Map KnobName Float

defineComponentKey ''KnobDefs
defineComponentKey ''KnobValues
defineComponentKey ''KnobState

initKnobsSystem :: MonadState ECS m => m ()
initKnobsSystem = do
    registerComponent "KnobDefs"   myKnobDefs   (newComponentInterface myKnobDefs)
    registerComponent "KnobState"  myKnobState  (newComponentInterface myKnobState)
    registerComponent "KnobValues" myKnobValues (savedComponentInterface myKnobValues)

addKnobDef :: (MonadState s m, MonadReader EntityID m, HasComponents s)
           => KnobName -> KnobScale -> Float -> (GLfloat -> EntityMonad ()) -> m ()
addKnobDef knobName scale defVal action = prependComponent myKnobDefs (Map.singleton knobName knobDef)
    where knobDef = KnobDef { knbScale = scale, knbDefault = defVal, knbAction = action }

-- Must use prependComponent rather than appendComponent to update the Map,
-- as its <> is left-biased
setKnobData :: (MonadState s m, MonadReader EntityID m, HasComponents s) => KnobName -> Float -> m ()
setKnobData knobName value = prependComponent myKnobValues (Map.singleton knobName value)

getKnobData :: (MonadState s m, MonadReader EntityID m, HasComponents s) => KnobName -> m Float
getKnobData knobName = do
    knobValues <- getComponentDefault mempty myKnobValues
    case Map.lookup knobName knobValues of
        Just value -> return value
        Nothing -> do
            knobDefs <- getComponentDefault mempty myKnobDefs
            case Map.lookup knobName knobDefs of
                Just knobDef -> return (knbDefault knobDef)
                Nothing      -> return 0





spawnActiveKnob name knobScale defVal action = do
    addKnobDef name knobScale defVal action

    i <- Map.size <$> getComponentDefault mempty myKnobDefs
    let x = fromIntegral (i `div` 4) * 0.4
        y = (3 - fromIntegral (i `mod` 4)) * 0.3 - 0.45
        knobPos = V3 (0.5 + x) y 0
    spawnActiveKnobAt knobPos name knobScale defVal action


spawnActiveKnobAt knobPos name knobScale defVal action = do

    let (low, high) = case knobScale of
            Linear low high -> (low, high)
            Stepped options -> (0, fromIntegral (length options) - 1)
        range = high - low

    let val01ToValue value01 = case knobScale of
            Linear _ _ -> low + range * value01
            -- E.g. for [foo,bar,baz] 0-0.33 should be 0, 0.33-0.66 should be 1, 0.66-1 should be 2
            Stepped _  -> fromIntegral . floor . min (range + 1 - 0.001) $ (range + 1) * value01
        valueToVal01 value = (value - low) / range
        displayValue value = case knobScale of
            Linear _ _      -> (printf "%.2f" (value::Float))
            Stepped options -> let i = round value in if i >= 0 && i < length options then options !! i else "<over>"
        initialRotation = value01ToKnobRotation (valueToVal01 defVal)

    initialValue01 <- getKnobData name
    let initialValue = val01ToValue initialValue01
    action initialValue
    nameLabel <- spawnChild $ do
        myText        ==> name
        myTextPose    ==> position (V3 0 0.1 0) !*! scaleMatrix 0.05
        myPose        ==> position knobPos
    valueLabel <- spawnChild $ do
        myText        ==> displayValue initialValue
        myTextPose    ==> position (V3 0 -0.1 0) !*! scaleMatrix 0.05
        myPose        ==> position knobPos

    parentID <- ask
    knob <- spawnChild $ do
        myShape        ==> Cube
        mySize         ==> 0.1
        myBody         ==> Animated
        myDragOverride ==> True
        myDragBegan ==> do
            withComponent_ myActiveDrag $ \(ActiveDrag _handEntityID startM44) -> do
                myKnobState ==% \k -> k { ksLastHandPose = startM44 }
        myDragContinues ==> \dragM44 -> do
            withComponent_ myActiveDrag $ \(ActiveDrag handEntityID _startM44) -> do
                -- Calculate the rotation for this tick
                newHandPose <- getEntityPose handEntityID
                oldState    <- getComponentDefault newKnobState myKnobState
                let diff = newHandPose `subtractMatrix` ksLastHandPose oldState
                    V3 dX _dY _dZ = testEpsilon $ quatToEuler (quaternionFromMatrix diff)
                    -- We want clockwise rotation, so bound to -2pi <> 0
                    newRotation = min 0 . max maxKnobRot $ ksRotation oldState - dX

                -- Update the knob's state and appearance
                myKnobState ==> (KnobState { ksLastHandPose = newHandPose, ksRotation = newRotation })
                setAttachmentOffset (positionRotation knobPos
                    (axisAngle (V3 0 0 1) newRotation) )

                -- Update the knob's label,
                -- and run the its action,
                -- with the scaled value
                let newValue01     = knobRotationToValue01 newRotation
                    newValueScaled = val01ToValue newValue01
                inEntity valueLabel $ setText (displayValue newValueScaled)
                action newValueScaled

                -- Record the knob value in the parent so it can be persisted
                inEntity parentID $ do
                    setKnobData name newValue01
        myDragEnded ==> do
            sceneWatcherSaveEntity parentID
        myKnobState ==> newKnobState { ksRotation = initialRotation }
    attachEntity knob
        (positionRotation knobPos
            (axisAngle (V3 0 0 1) initialRotation))
    return knob


knobRotationToValue01 rot = rot / maxKnobRot
value01ToKnobRotation val = val * maxKnobRot

twoPi = 2 * pi
maxKnobRot = -twoPi * 3 / 4

testEpsilon n = if nearZero n then 0 else n
