module Eraser where
import Rumpus

start :: Start
start = do

    thisID <- ask
    handIDs <- getHandIDs
    let excluded = thisID:handIDs
    eraser <- spawnChild $ do
        myBody ==> Animated
        mySize ==> 0.1
        myCollisionBegan ==> \entityID _ ->
            unless (entityID `elem` excluded) $ do
                sceneWatcherRemoveEntity entityID
    attachEntity eraser (position $ V3 0 0.5 0)