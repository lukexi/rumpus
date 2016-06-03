module Seed where
import Rumpus

gestationPeriod = 3

start :: Start
start = do
    myCodeHidden ==> True
    setShape Sphere
    setSize 0.2
    setColor (V4 0.2 0.3 0.1 1)

    birthTime <- getNow
    myCollisionStart ==> \_ _ -> do
        isHeld <- isBeingHeld
        now    <- getNow
        when (not isHeld && (now - birthTime) > gestationPeriod) $ do
            removeComponent myCollisionStart

            _treeID <- spawnChildInstance "Tree"

            setShape Cube
            animateSizeTo (V3 0.4 0.1 0.4) 1

    return ()
