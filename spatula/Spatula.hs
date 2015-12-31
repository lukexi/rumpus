{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
module Spatula where

import Control.Lens.Extra
import Linear.Extra
import Graphics.GL.Pal
import Graphics.VR.Pal
import Control.Monad.State
import Control.Monad.Random
import Types
import Entity
import Control

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
    return (leftHand:rightHand:aSpatula:planes)

leftHand :: Entity
leftHand = newEntity 
        { _entColor       = V4 1 0.8 0.7 1 
        , _entSize        = V3 0.1 0.1 0.4
        -- , _entPhysProps   = [IsKinematic, IsGhost]
        , _entPhysProps   = [IsKinematic]
        , _entUpdate      = Just $ \entityID -> 
            withLeftHandEvents $ \case
                HandStateEvent hand -> 
                    setEntityPose entityID (poseFromMatrix (hand ^. hndMatrix))
                HandButtonEvent HandButtonTrigger ButtonDown -> do
                    setEntityColor entityID (V4 1 0 1 1)
                HandButtonEvent HandButtonTrigger ButtonUp -> do
                    setEntityColor entityID (V4 0 0 1 1)
                _ -> return ()
        }

rightHand :: Entity
rightHand = newEntity 
        { _entColor       = V4 0.7 1 0.8 1 
        , _entSize        = V3 0.1 0.1 0.4
        -- , _entPhysProps   = [IsKinematic, IsGhost]
        , _entPhysProps   = [IsKinematic]
        , _entUpdate      = Just $ \entityID -> 
            withRightHandEvents $ \case
                HandStateEvent hand -> 
                    setEntityPose entityID (poseFromMatrix (hand ^. hndMatrix))
                HandButtonEvent HandButtonTrigger ButtonDown -> do
                    setEntityColor entityID (V4 1 1 1 1)
                HandButtonEvent HandButtonTrigger ButtonUp -> do
                    setEntityColor entityID (V4 0 1 0 1)
                _ -> return ()
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
        , _entPhysProps   = [IsKinematic]
        , _entShape       = Cube
        , _entUpdate      = Just $ \entityID -> do
            now <- getNow
            let a     = (*20) . sin . (/10) $ now
                spatX = (*a) . sin  $ now
                spatZ = (*a) . cos  $ now
                newPose_ = Pose (V3 spatX 0.1 spatZ) (axisAngle (V3 0 1 0) (now + (pi/2)))
            setEntityPose entityID newPose_
        }
