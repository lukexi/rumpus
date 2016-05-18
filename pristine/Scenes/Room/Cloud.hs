module Cloud where

start = do
    cloudID <- ask
    let numPuffs = 5
	forM_ [0..numPuffs] $ \i ->
        hue <- randomRIO (0.3, 0.5)
        x <- (fromIntegral i / numPuffs +) <$> randomRIO (0,0.2)
        y <- randomRIO (-0.1,0.1)
        z <- randomRIO (-0.1,0.1)
        puffID <- spawnChild $ do
            myShape ==> Sphere
            myProperties ==> [Holographic]
            myColor ==> colorHSL hue 0.5 0.5
            myPose ==> translateMatrix (V3 x y z)
            myUpdate ==> do
                now <- (i +) <$> getNow
                setSize ((sin now / 2 + 1) * 0.2 + 0.2) -- 0.2<->0.4
        inEntity puffID $ setRepeatingAction (1/4) $ do
            chance <- randomRIO (0,3)
            when (chance == 0) $ do
                cloudPose <- getEntityPose cloudID
                startPos <- V3 <$> randomRIO (0, numPuffs)
                               <*> randomRIO (-0.1, 0.1)
                               <*> randomRIO (-0.1, 0.1)
                hue <- randomRIO (0,1)
                spawnChild $ do
                    myShape ==> Sphere
                    myPose ==> cloudPose !*! translateMatrix startPos
                    mySize ==> 0.1
                    myColor ==> colorHSL hue 0.5 0.8
