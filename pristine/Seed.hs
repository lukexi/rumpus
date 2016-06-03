module Seed where
import Rumpus


start :: Start
start = do
    myCodeHidden ==> True
    setShape Sphere
    setSize 0.2
    setColor (V4 0.2 0.3 0.1 1)
    setFloating False

    myCollisionStart ==> \_ _ -> do
        isHeld <- isBeingHeld
        when (not isHeld) $ do
            removeComponent myCollisionStart

            _treeID <- spawnChildInstance "Tree"

            setRotation (V3 0 1 0) 0
            setShape Cube
            animateSizeTo (V3 0.4 0.1 0.4) 1

    return ()
