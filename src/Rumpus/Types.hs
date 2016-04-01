{-# LANGUAGE CPP #-}

module Rumpus.Types where
import Data.ECS

type HandEntityID = EntityID

isInReleaseMode :: Bool
#if defined(RUMPUS_RELEASE)
isInReleaseMode = True
#else
isInReleaseMode = False
#endif