{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Spatula where

import Control.Lens.Extra
import Linear.Extra
import Graphics.GL.Pal
import Physics.Bullet
import Control.Monad.State
import System.Random
import Control.Monad.Reader
import Entity

createPlaneMess :: (MonadIO m, MonadReader WorldStatic m, MonadState World m) => m ()
createPlaneMess = do
    -- Create a mess of planes
    let planeSize = V3 1 1 0.1
    forM_ [1..1000::Int] $ \_ -> do
        [r,g,b] <- liftIO (replicateM 3 randomIO)
        entityID <- createEntity $ newEntity
            { _entColor = V4 r g b 1
            , _entPose = newPose & posPosition .~ V3 0 20 0
            , _entSize = planeSize
            }
        addEntityRigidBodyComponent entityID IsNotKinematic


createSpatula :: (MonadIO m, MonadReader WorldStatic m, MonadState World m) => m ()
createSpatula = do
    entityID <- createEntity $ newEntity
        { _entSize        = V3 2 1 1
        , _entPose        = newPose & posPosition .~ V3 0 0.5 0
        , _entColor       = V4 0 1 1 1
        , _entUpdate      = Just $ \entity -> do
            now <- getNow
            let a     = (*20) . sin . (/10) $ now
                spatX = (*a) . sin  $ now
                spatZ = (*a) . cos  $ now
                _ = spatX :: Double
            forM_ (entity ^. entRigidBody) $ \rigidBody ->
                setRigidBodyWorldTransform rigidBody
                    (V3 spatX 0.4 spatZ) 
                    (axisAngle (V3 0 1 0) (now + (pi/2)))
            return entity
        }
    addEntityRigidBodyComponent entityID IsKinematic
