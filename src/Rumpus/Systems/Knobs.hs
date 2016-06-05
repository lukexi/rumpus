module Rumpus.Systems.Knobs where
import PreludeExtra
import Rumpus.Systems.Shared
import Rumpus.Systems.Drag
import qualified Data.HashMap.Strict as Map
type KnobName = String

data Knob = Knob
    { knbName   :: KnobName
    , knbRange  :: (Float, Float)
    , knbValue  :: Float
    , knbAction :: GLfloat -> EntityMonad ()
    }

type Knobs = [Knob]
type KnobsData = Map KnobName Float

defineComponentKey ''Knobs
defineComponentKey ''KnobsData

initKnobsSystem :: MonadState ECS m => m ()
initKnobsSystem = do
    registerComponent "Knobs" myKnobs (newComponentInterface myKnobs)
    registerComponent "KnobsData" myKnobsData (savedComponentInterface myKnobsData)

-- | E.g.
-- > addQuickKnob "Scale" (0.1, 10) setSize
addQuickKnob :: String -> (Float, Float) -> (GLfloat -> EntityMonad ()) -> EntityMonad ()
addQuickKnob name (low, high) action = do
    savedValue <- fromMaybe 0 . Map.lookup name . fromMaybe mempty <$> getComponent myKnobsData
    action savedValue
    
    _knobID <- spawnEntity $ do
        myShape ==> Cube
        myDrag ==> \changeM44 ->
            action (changeM44 ^. translation . _x)
    let knob = Knob
            { knbName = name
            , knbRange = (low, high)
            , knbValue = 0
            , knbAction = action
            }
    appendComponent myKnobs [knob]

