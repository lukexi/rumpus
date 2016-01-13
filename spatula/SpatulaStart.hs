import Rumpus.Systems.Shared
import Control.Lens.Extra
import Rumpus.Types
import Sound.Pd
import Data.Dynamic

start :: OnStart
start entityID = do
    chan <- traverseM (use (wldComponents . cmpPdPatch . at entityID)) $ \patch -> do
        pd <- view wlsPd
        toDyn <$> makeReceiveChan pd (local patch "freq")
    return chan
