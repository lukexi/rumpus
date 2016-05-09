{-# LANGUAGE ViewPatterns #-}

module RumpusLib where
import PreludeExtra

createNewTimer :: (RealFrac a, MonadIO m) => a -> m (TVar Bool)
createNewTimer ticksPerSec = liftIO $ registerDelay (floor $ (1/ticksPerSec) * 1000 * 1000)

checkTimer :: MonadIO m => TVar Bool -> m Bool
checkTimer = liftIO . atomically . readTVar

randomFrom :: MonadIO m => [a] -> m a
randomFrom list = do
    i <- liftIO (randomRIO (0, length list - 1))
    return (list !! i)

-- | Points distributed evenly in a sphere
-- (via http://www.softimageblog.com/archives/115)
goldenSectionSpiralPoints :: Int -> [V3 GLfloat]
goldenSectionSpiralPoints (fromIntegral -> n) =
    let inc = pi * (3 - sqrt 5)
        off = 2 / n
    in flip map [0..n] $ \k ->
        let y = k * off - 1 + (off / 2)
            r = sqrt (1 - y*y)
            phi = k * inc
        in V3 (cos phi * r) y (sin phi * r)
