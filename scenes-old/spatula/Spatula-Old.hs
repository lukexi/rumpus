{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Spatula where

import PreludeExtra
import Rumpus

initScene :: (MonadIO m, MonadState ECS m, MonadReader WorldStatic m) => m ()
initScene = do
    defineEntity leftHand
    defineEntity rightHand
    defineEntity soundCube
    defineEntity messyCube
    defineEntity messyBall
    defineEntity spatula
    defineEntity theFloor

    _ <- spawnEntity Persistent "Left Hand"
    _ <- spawnEntity Persistent "Right Hand"
    _ <- spawnEntity Persistent "Floor"
    _ <- spawnEntity Persistent "Spatula"
    _ <- spawnEntity Persistent "MessyCube"
    _ <- spawnEntity Persistent "MessyBall"
    _ <- spawnEntity Persistent "SoundCube"

    
    return ()

leftHand :: Entity
leftHand = newEntity 
        { _entColor       = V4 1 0.8 0.7 1 
        , _entSize        = V3 0.1 0.1 0.4
        , _entShape       = CubeShape
        , _entPose        = newPose & posPosition .~ (V3 0 0.1 0)
        , _entPhysicsProperties   = [NoContactResponse]
        , _entName        = "Left Hand"
        }

rightHand :: Entity
rightHand = newEntity 
        { _entColor       = V4 0.7 1 0.8 1 
        , _entSize        = V3 0.1 0.1 0.4
        , _entShape       = CubeShape
        , _entPhysicsProperties   = [NoContactResponse]
        , _entName        = "Right Hand"
        }

messyCube :: Entity
messyCube = newEntity
            { _entPose = newPose & posPosition .~ V3 0 2 0
            , _entSize = 0.3
            , _entShape = CubeShape
            , _entName  = "MessyCube"
            }

soundCube :: Entity
soundCube = newEntity
            { _entPose = newPose & posPosition .~ V3 0 2 0
            , _entSize = 0.5
            , _entShape = CubeShape
            , _entName  = "SoundCube"
            , _entPdPatch = Just "spatula/spatula1"
            }


messyBall :: Entity
messyBall = newEntity
            { _entPose = newPose & posPosition .~ V3 1 2 0
            , _entSize = 0.3
            , _entShape = SphereShape
            , _entName  = "MessyBall"
            }



spatula :: Entity
spatula = newEntity
        { _entSize        = V3 0.2 0.1 0.1
        , _entPose        = newPose & posPosition .~ V3 0 0.5 0
        , _entColor       = V4 0 1 1 1
        , _entPhysicsProperties   = [Kinematic]
        , _entShape       = CubeShape
        , _entName        = "Spatula"
        , _entOnUpdate    = Just "spatula/Wobble.hs"
        , _entPdPatch     = Just "spatula/spatula2"
        }

theFloor :: Entity
theFloor = newEntity
        { _entPose = newPose & posOrientation .~ axisAngle (V3 1 0 0) ((-pi)/2)
        , _entMass = 0
        , _entShape = StaticPlaneShape
        , _entSize = V3 1000 1000 1
        , _entName = "Floor"
        }




attachWithSpring :: (MonadState ECS m, MonadIO m, MonadReader WorldStatic m) => EntityID -> m ()
attachWithSpring entityID = do
    withRightHandEvents $ \case
        HandStateEvent hand -> do
            let pose = poseFromMatrix (hand ^. hndMatrix)
            setEntityPose pose entityID

            useTraverseM_ (wldComponents . cmpSpring . at entityID) $ \spring ->
                setSpringWorldPose spring (pose ^. posPosition) (pose ^. posOrientation)
        HandButtonEvent HandButtonTrigger ButtonDown -> do

            overlapping         <- getEntityOverlapping entityID
            nonFloorOverlapping <- flip filterM overlapping $ \overlapper -> do
                overlapperEntityID <- unCollisionObjectID <$> getCollisionObjectID overlapper
                (/= Just "Floor") <$> use (wldComponents . cmpName . at overlapperEntityID)
            forM_ (listToMaybe nonFloorOverlapping) $ \oneNonFloor -> do
                do
                    anEntityID <- unCollisionObjectID <$> getCollisionObjectID oneNonFloor
                    printIO =<< use (wldComponents . cmpName . at anEntityID)
                dynamicsWorld <- view wlsDynamicsWorld

                spring <- addWorldSpringConstraint dynamicsWorld (RigidBody oneNonFloor)
                wldComponents . cmpSpring . at entityID ?= spring

                setSpringLinearLowerLimit  spring (-5  :: V3 Float)
                setSpringLinearUpperLimit  spring (5   :: V3 Float)
                setSpringAngularLowerLimit spring (-1  :: V3 Float)
                setSpringAngularUpperLimit spring (1   :: V3 Float)
                setSpringAngularStiffness  spring (100 :: V3 Float)
                setSpringLinearStiffness   spring (100 :: V3 Float)
                setSpringLinearDamping     spring (0.9 :: V3 Float)
                setSpringAngularDamping    spring (0.9 :: V3 Float)
                setSpringLinearBounce      spring (10  :: V3 Float)
                setSpringAngularBounce     spring (10  :: V3 Float)
                -- setSpringLinearEquilibrium spring 0
                -- setSpringAngularEquilibrium spring 0
                setLinearSpringEnabled spring (V3 True True True)
                setAngularSpringEnabled spring (V3 True True True)
                return ()
            setEntityColor (V4 1 1 1 1) entityID
        HandButtonEvent HandButtonTrigger ButtonUp -> do
            setEntityColor (V4 0 1 0 1) entityID

            useTraverseM_ (wldComponents . cmpSpring . at entityID) $ \spring -> do
                dynamicsWorld <- view wlsDynamicsWorld
                removeSpringConstraint dynamicsWorld spring
                wldComponents . cmpSpring . at entityID .= Nothing
        _ -> return ()
