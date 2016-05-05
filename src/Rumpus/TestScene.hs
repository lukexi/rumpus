{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Rumpus.TestScene where

import Rumpus

-- | For profiling (where we can't run code)
-- and for other prototyping.

loadTestScene :: ECSMonad ()
loadTestScene = do
    -- testEntity <- spawnEntity $ return ()
    -- addCodeExpr testEntity "CollisionStart" "collisionStart" myCollisionStartExpr myCollisionStart
    -- selectEntity testEntity


    -- Spawn objects
    forM_ [room, city, fountain] $ \onStart -> do
    --forM_ [room] $ \onStart -> do
        spawnEntity $ do
            myStart ==> onStart
            mySize ==> 0.3
            myShape ==> Cube

    setWorldPlaying True

    return ()

------------------------------------------------------------------------------
-- Room
roomCube = 4
(roomW, roomH, roomD) = (roomCube,roomCube,roomCube)
wallD = 1
shelfH = 0.15

roomOffset = (roomH/2 - wallD/2)

room :: Start
room = do
    setPose (identity & translation .~ V3 0 roomOffset (-roomD/2 + 0.4))
    removeChildren
    builderID <- ask
    let makeWall pos size hue = spawnEntity $ do
            myParent            ==> builderID
            myPose              ==> mkTransformation
                (axisAngle (V3 0 0 1) 0) (pos & _y +~ roomOffset)
            myShape         ==> Cube
            myProperties ==> [Floating, Static]
            mySize              ==> size
            myColor             ==> colorHSL hue 0.8 0.6
            myMass              ==> 0
    --makeWall (V3 0 0 (-roomD/2)) (V3 roomW roomH wallD) 0.1 -- back
    --makeWall (V3 0 0 (roomD/2))  (V3 roomW roomH wallD) 0.2 -- front
    --makeWall (V3 (-roomW/2) 0 0) (V3 wallD roomH roomD) 0.3 -- left
    --makeWall (V3 (roomW/2)  0 0) (V3 wallD roomH roomD) 0.4 -- right
    makeWall (V3 0 (-roomH/2) 0) (V3 roomW wallD roomD) 0.5 -- floor
    makeWall (V3 0 (roomH/2)  0) (V3 roomW wallD roomD) 0.6 -- ceiling

    let numShelves = 4
    forM_ [1..(numShelves - 1)] $ \n -> do
        let shelfY = (roomH/realToFrac numShelves)
                        * fromIntegral n - (roomH/2)
        makeWall (V3 0 shelfY (roomD/2))
                 (V3 roomW shelfH (wallD*2)) 0.7 -- shelf


-------------------------------------------------------------------------------
-- City

-- Golden Section Spiral
-- (via http://www.softimageblog.com/archives/115)
pointsOnSphere (fromIntegral -> n) =
    let inc = pi * (3 - sqrt 5)
        off = 2 / n
    in flip map [0..n] $ \k ->
        let y = k * off - 1 + (off / 2)
            r = sqrt (1 - y*y)
            phi = k * inc
        in V3 (cos phi * r) y (sin phi * r)

city :: Start
city = do
    removeChildren
    createBuildings
    createStars

createBuildings :: EntityMonad ()
createBuildings = do
    rootEntityID <- ask
    let n = 8
        dim = 200
        height = dim * 5
        buildSites = [V3 (x * dim * 2) (-height) (z * dim * 2) | x <- [-n..n], z <- [-n..n]]
    forM_ (zip [0..] buildSites) $ \(i, V3 x y z) -> do
        hue <- liftIO randomIO

        when (x /= 0 && z /= 0) $ void . spawnEntity $ do
            myParent               ==> rootEntityID
            myPose                 ==> mkTransformation
                                            (axisAngle (V3 0 0 1) 0) (V3 x y z)
            myShape            ==> Cube
            myProperties    ==> [Holographic]
            --myUpdate ==> do
            --    now <- getNow
            --    let newHeight = ((sin (now+i) + 1) + 1) * height
            --    setSize (V3 dim newHeight dim)
            mySize                 ==> V3 dim height dim
            myColor                ==> colorHSL hue 0.8 0.8

createStars :: EntityMonad ()
createStars = do
    rootEntityID <- ask

    let numPoints = 300 :: Int
        sphere = pointsOnSphere numPoints
        hues = map ((/ fromIntegral numPoints) . fromIntegral) [0..numPoints]
    forM_ (zip sphere hues) $ \(pos, hue) -> void $ spawnEntity $ do
        myParent               ==> rootEntityID
        myPose                 ==> mkTransformation
                                        (axisAngle (V3 0 0 1) 0.3) (pos * 1000)
        myShape            ==> Sphere
        myProperties    ==> [Holographic]
        mySize                 ==> 5
        myColor                ==> colorHSL hue 0.8 0.8

-------------------------
-- Fountain

rate :: Float
rate = 10

majorScale = map (+60) [0,2,4,7,9]

fountain :: Start
fountain = do
    removeChildren

    myUpdate ==> do
        withState $ \timer -> do
            shouldSpawn <- checkTimer timer
            when shouldSpawn $ do
                -- Play a note
                note <- randomFrom majorScale
                sendPd "note" (Atom $ realToFrac note)

                -- Spawn a ball
                pose <- getPose
                spawnEntity $ do
                    myPose      ==> pose & translation +~
                                        (pose ^. _m33) !* (V3 0 0.3 0)
                    myShape ==> Sphere
                    mySize      ==> 0.03
                    myMass      ==> 0.1
                    myColor     ==> colorHSL (note / 12) 0.9 0.8
                    myStart     ==> do
                        setLifetime 10
                        applyForce $ (pose ^. _m33) !* (V3 0 0.3 0)

                -- Create a new timer
                newTimer <- createNewTimer rate
                setState newTimer

    -- Create the initial timer
    newTimer <- createNewTimer rate
    setState newTimer
