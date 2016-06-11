module Rumpus.Systems.Knobs where
import PreludeExtra
import Rumpus.Systems.Shared
import Rumpus.Systems.Drag
import qualified Data.HashMap.Strict as Map
type KnobName = String

data Knob = Knob
    { knbName   :: KnobName
    , knbRange  :: (Float, Float)
    , knbAction :: GLfloat -> EntityMonad ()
    }

type Knobs = [Knob]
type KnobValues = Map KnobName Float

defineComponentKey ''Knobs
defineComponentKey ''KnobValues

initKnobsSystem :: MonadState ECS m => m ()
initKnobsSystem = do
    registerComponent "Knobs" myKnobs (newComponentInterface myKnobs)
    registerComponent "KnobValues" myKnobValues (savedComponentInterface myKnobValues)

-- | E.g.
-- > addQuickKnob "Scale" (0.1, 10) setSize
addQuickKnob :: String -> (Float, Float) -> (GLfloat -> EntityMonad ()) -> EntityMonad ()
addQuickKnob name (low, high) action = do
    savedValue <- Map.lookupDefault 0 name <$> getComponentDefault mempty myKnobValues
    action savedValue

    _knobID <- spawnEntity $ do
        myShape ==> Cube
        myDrag ==> \changeM44 -> do
            let newValue = changeM44 ^. translation . _x
            myKnobValues ==% (at name ?~ newValue)
            action newValue
    let knob = Knob
            { knbName = name
            , knbRange = (low, high)
            , knbAction = action
            }
    appendComponent myKnobs [knob]


-- Must use prependComponent rather than appendComponent to update the Map,
-- as its <> is left-biased
setKnobData :: (MonadState s m, MonadReader EntityID m, HasComponents s) => KnobName -> Float -> m ()
setKnobData knobName value = prependComponent myKnobValues (Map.singleton knobName value)

getKnobData :: (MonadState s m, MonadReader EntityID m, HasComponents s) => KnobName -> Float -> m Float
getKnobData knobName defVal = Map.lookupDefault defVal knobName <$> getComponentDefault mempty myKnobValues
