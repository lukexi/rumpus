module Rumpus.Systems.Hands where

data HandsSystem = HandsSystem 
    { _hndLeftHand  :: EntityID
    , _hndRightHand :: EntityID
    , _hndHead      :: EntityID
    } deriving Show
makeLenses ''HandsSystem

defineSystemKey ''HandsSystem

tickHandsSystem
