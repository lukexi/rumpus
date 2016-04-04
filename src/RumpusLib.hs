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
