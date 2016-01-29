{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Animation where
import PreludeExtra
import Rumpus.Types
import Rumpus.Systems.Shared
import Rumpus.Systems.Physics
import qualified Data.Map as Map

animationSystem :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => m ()
animationSystem = do
    now <- getNow
    traverseM_ (Map.toList <$> use (wldComponents . cmpAnimationColor)) $ \(entityID, animation) -> do
        let evaled = evalAnim now animation

        wldComponents . cmpColor . ix entityID .= evanResult evaled
        when (evanRunning evaled == False) $ do
            wldComponents . cmpAnimationColor . at entityID .= Nothing

    traverseM_ (Map.toList <$> use (wldComponents . cmpAnimationSize)) $ \(entityID, animation) -> do
        let evaled = evalAnim now animation

        setEntitySize (evanResult evaled) entityID
        when (evanRunning evaled == False) $ do
            wldComponents . cmpAnimationSize . at entityID .= Nothing
