module StartAnalyzer where
import Rumpus

start :: Start
start = do
    receiveChan <- withPdPatch $ \patch -> do
        pd <- viewSystem sysSound sndPd
        makeReceiveChan pd (local patch "volume-note")
    return (toDyn <$> receiveChan)
