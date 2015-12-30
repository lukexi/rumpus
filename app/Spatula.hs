{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Spatula where

import Control.Lens.Extra
import Linear.Extra
import Graphics.GL.Pal
import Control.Monad.State
import Control.Monad.Random
import Entity

-- import System.Random
-- instance Random r => Random (V3 r) where
--     randomR (a,b) g = flip runRand g $ do
--         x <- getRandomR (a,b)
--         y <- getRandomR (a,b)
--         z <- getRandomR (a,b)
--         return (V3 x y z)

initScene :: (MonadIO m) => m [Entity]
initScene = do

    -- stdgen <- liftIO getStdGen

    planes <- createPlaneMess
    return (aHand:aSpatula:planes)

aHand :: Entity
aHand = newEntity 
        { _entColor       = V4 1 0.9 0.8 1 
        , _entSize        = V3 0.1 0.1 0.4
        , _entPhysProps   = [IsKinematic, IsGhost]
        , _entUpdate      = Just $ \entityID -> do
            setEntityPose entityID newPose
        }

createPlaneMess :: MonadIO m => m [Entity]
createPlaneMess = do
    -- Create a mess of planes
    let planeSize = V3 0.1 0.1 0.01
    forM [1..100::Int] $ \_ -> do
        [r,g,b] <- liftIO (replicateM 3 (randomRIO (0,1)))
        return $ newEntity
            { _entColor = V4 r g b 1
            , _entPose = newPose & posPosition .~ V3 0 20 0
            , _entSize = planeSize
            , _entShape = Cube
            }

aSpatula :: Entity
aSpatula = newEntity
        { _entSize        = V3 0.2 0.1 0.1
        , _entPose        = newPose & posPosition .~ V3 0 0.5 0
        , _entColor       = V4 0 1 1 1
        , _entUpdate      = Just $ \entityID -> do
            now <- getNow
            let a     = (*20) . sin . (/10) $ now
                spatX = (*a) . sin  $ now
                spatZ = (*a) . cos  $ now
                newPose_ = Pose (V3 spatX 0.1 spatZ) (axisAngle (V3 0 1 0) (now + (pi/2)))
            setEntityPose entityID newPose_
        , _entPhysProps = [IsKinematic]
        , _entShape = Cube
        }
