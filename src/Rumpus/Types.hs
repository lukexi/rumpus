{-# LANGUAGE CPP #-}

module Rumpus.Types where
import Control.Monad.Trans
import Graphics.GL.Pal
import Data.IORef
import Data.Time

versionString :: String
versionString = "0.1.5"

isInReleaseMode :: Bool
#if defined(RUMPUS_RELEASE)
isInReleaseMode = True
#else
isInReleaseMode = False
#endif

isBeingProfiled :: Bool
#if defined(RUMPUS_PROFILE)
isBeingProfiled = True
#else
isBeingProfiled = False
#endif

profileMS' :: (MonadIO m) => String -> Int -> m a -> m a
profileMS' = profileMS
--profileMS' _ _ = id
--profileMS' name _ act = putStrLnIO ("About to run " ++ name ++ "...") >> act

profileFPS' :: (MonadIO m) => String -> Int -> m a -> m a
--profileFPS' = profileFPS
profileFPS' _ _ = id




makeCheckFPS :: MonadIO m => m (m ())
makeCheckFPS = do
    fpsRef <- liftIO . newIORef =<< liftIO getCurrentTime
    let checkFPS = liftIO $ do
            now    <- getCurrentTime
            before <- readIORef fpsRef
            writeIORef fpsRef now
            let timeDiff = now `diffUTCTime` before
            putStrLn ("FPS: " ++ show (1/timeDiff))
    return checkFPS
