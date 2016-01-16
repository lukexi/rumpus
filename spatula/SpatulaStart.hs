module SpatulaStart where
import Rumpus

start :: OnStart
start entityID = do
    chan <- traverseM (use (wldComponents . cmpPdPatch . at entityID)) $ \patch -> do
        pd <- view wlsPd
        toDyn <$> makeReceiveChan pd (local patch "freq")
    return chan
