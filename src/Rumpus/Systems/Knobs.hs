module Rumpus.Systems.Knobs where
import PreludeExtra
import Rumpus.Systems.Shared
import Rumpus.Systems.Drag

type KnobName = String

data Knob = Knob
    { knbName   :: KnobName
    , knbRange  :: (Float, Float)
    , knbValue  :: Float
    , knbAction :: M44 GLfloat -> EntityMonad ()
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
addQuickKnob :: (MonadIO m, MonadState ECS m, MonadReader EntityID m)
             => String -> (Float, Float) -> (M44 GLfloat -> EntityMonad ()) -> m ()
addQuickKnob name (low, high) action = do
    _knobID <- spawnEntity $ do
        myShape ==> Cube
        myDrag ==> \changeM44 -> do
            action changeM44
    let knob = Knob
            { knbName = name
            , knbRange = (low, high)
            , knbValue = 0
            , knbAction = action
            }
    appendComponent myKnobs [knob]

