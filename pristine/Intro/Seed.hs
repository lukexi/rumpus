module Seed where
import Rumpus


start :: Start
start = do
    myCodeHidden ==> True
    setShape Sphere
    setSize 0.2
    setColor (V4 0.2 0.3 0.1 1)
    setBody Physical
    handIDs <- getHandIDs
    myCollisionBegan ==> \hitID _ -> do
        let notHand = hitID `notElem` handIDs
        isHeld <- isBeingHeld
        when (notHand && not isHeld) $ do
            removeComponent myCollisionBegan

            _treeID <- spawnChildInstance "Tree"

            setRotation (V3 0 1 0) 0
            setShape Cube
            animateSizeTo (V3 0.4 0.1 0.4) 1

    return ()