{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Animation where
import PreludeExtra
import Rumpus.Types
import qualified Data.Map as Map

animationSystem :: (MonadIO m, MonadState World m) => m ()
animationSystem = do
    now <- getNow
    colorAnimations <- Map.toList <$> use (wldComponents . cmpAnimationColor)
    forM_ colorAnimations $ \(entityID, animation) -> do
        let evaled = evalAnim now animation

        wldComponents . cmpColor . ix entityID .= evanResult evaled
        when (evanRunning evaled == False) $ do
            wldComponents . cmpAnimationColor . at entityID .= Nothing
