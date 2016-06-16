
module DoorMaker where
import Rumpus

start :: Start
start = do


spawnChildInstance :: (MonadBaseControl IO m, MonadIO m, MonadState ECS m, MonadReader EntityID m) => FilePath -> m EntityID
spawnChildInstance = do
    let codePath = "Door" <.> "hs"
    spawnChild $ do
        myStartExpr   ==> (codePath, "start")
        myCodeHidden  ==> True
