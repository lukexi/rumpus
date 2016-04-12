module SpatulaStart where
import Rumpus

start :: Start
start entityID = do
    chan <- traverseM (use (wldComponents . myPdPatch . at entityID)) $ \patch -> do
        pd <- view wlsPd
        toDyn <$> makeReceiveChan pd (local patch "freq")
    return chan
