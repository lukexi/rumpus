module Seed where
import Rumpus

gestationPeriod = 3

start :: Start
start = do

    birthTime <- getNow
    myCollisionStart ==> \_ _ -> do
        isHeld <- isBeingHeld
        now    <- getNow
        when (not isHeld && (now - birthTime) > gestationPeriod) $ do
            removeComponent myCollisionStart
            printIO "BEGIN GROWING"
    return ()
