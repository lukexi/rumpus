{-# LANGUAGE CPP #-}

module Rumpus.Types where
import Data.ECS
import Control.Monad.Trans
import Graphics.GL.Pal
type HandEntityID = EntityID

isInReleaseMode :: Bool
#if defined(RUMPUS_RELEASE)
isInReleaseMode = True
#else
isInReleaseMode = False
#endif

profileMS' :: (MonadIO m) => String -> Int -> m a -> m a
--profileMS' = profileMS
profileMS' _ _ = id
--profileMS' name _ act = putStrLnIO ("About to run " ++ name ++ "...") >> act 

profileFPS' :: (MonadIO m) => String -> Int -> m a -> m a
--profileFPS' = profileFPS
profileFPS' _ _ = id