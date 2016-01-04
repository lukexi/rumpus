{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Physics.Bullet
import Sound.Pd
-- import qualified Data.Set as Set
import Data.Maybe
-- import Control.Concurrent

initScene :: (MonadIO m) => m [Entity]
initScene = do

    soundBlocks  <- liftIO (evalRandIO createSoundBlocks)
    -- someBalls  <- liftIO (evalRandIO createBallMess)
    -- somePlanes <- liftIO (evalRandIO createPlaneMess)
    -- return (leftHand:rightHand:aFloor:aSpatula:somePlanes ++ someBalls)
    -- return (leftHand:rightHand:aFloor:soundBlocks ++ somePlanes ++ someBalls)
    return (leftHand:rightHand:aFloor:soundBlocks)

leftHand :: Entity
leftHand = newEntity 
        { _entColor       = V4 1 0.8 0.7 1 
        , _entSize        = V3 0.1 0.1 0.4
        , _entShape       = CubeShape
        , _entPose        = newPose & posPosition .~ (V3 0 0.1 0)
        , _entPhysProps   = [IsGhost]
        , _entName        = "Left Hand"
        -- , _entPhysProps   = [IsKinematic]
        , _entUpdate      = Just $ \entityID -> do
            now <- getNow
            setEntityPose entityID (newPose & posPosition .~ V3 (sin now) 0.1 0)
            withEntityGhostObject entityID $ \ghostObject -> do
                overlapping    <- getGhostObjectOverlapping ghostObject
                overlappingIDs <- mapM getCollisionObjectID overlapping
                -- printIO overlappingIDs
                forM_ (map unCollisionObjectID overlappingIDs) $ \touchedID -> do
                    name <- fromMaybe "Entity" <$> use (wldComponents . cmpName . at touchedID)
                    when (name /= "Floor") $ do
                        randomColor <- (_w .~ 1) <$> liftIO (randomRIO (0,1))
                        setEntityColor touchedID randomColor
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
        , _entShape       = CubeShape
        , _entPhysProps   = [IsGhost]
        , _entName        = "Right Hand"
        -- , _entPhysProps   = [IsKinematic]
        , _entUpdate      = Just $ \entityID -> do
            withRightHandEvents $ \case
                HandStateEvent hand -> do
                    let pose = poseFromMatrix (hand ^. hndMatrix)
                    setEntityPose entityID pose

                    useMaybeM_ (wldComponents . cmpSpring . at entityID) $ \spring ->
                        setSpringWorldPose spring (pose ^. posPosition) (pose ^. posOrientation)
                HandButtonEvent HandButtonTrigger ButtonDown -> do

                    withEntityGhostObject entityID $ \ghostObject -> do
                        overlapping    <- getGhostObjectOverlapping ghostObject
                        nonFloorOverlapping <- flip filterM overlapping $ \overlapper -> do
                            overlapperEntityID <- unCollisionObjectID <$> getCollisionObjectID overlapper
                            (/= Just "Floor") <$> use (wldComponents . cmpName . at overlapperEntityID)
                        forM_ (listToMaybe nonFloorOverlapping) $ \oneNonFloor -> do
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
                    setEntityColor entityID (V4 1 1 1 1)
                HandButtonEvent HandButtonTrigger ButtonUp -> do
                    setEntityColor entityID (V4 0 1 0 1)

                    useMaybeM_ (wldComponents . cmpSpring . at entityID) $ \spring -> do
                        dynamicsWorld <- view wlsDynamicsWorld
                        removeSpringConstraint dynamicsWorld spring
                        wldComponents . cmpSpring . at entityID .= Nothing
                _ -> return ()
        }

createPlaneMess :: MonadRandom m => m [Entity]
createPlaneMess = do
    -- Create a mess of planes
    -- let planeSize = V3 0.2 0.2 0.1
    let planeSize = 0.3
    forM [1..100::Int] $ \_ -> do
        color <- getRandomR (0,1)
        return $ newEntity
            { _entColor = color & _w .~ 1
            , _entPose = newPose & posPosition .~ V3 0 20 0
            , _entSize = planeSize
            , _entShape = CubeShape
            , _entName  = "MessyCube"
            }

createSoundBlocks :: MonadRandom m => m [Entity]
createSoundBlocks = do
    -- Create a mess of planes
    -- let planeSize = V3 0.2 0.2 0.1
    let planeSize = 0.5
    forM [1..8::Int] $ \i -> do
        color <- getRandomR (0,1)
        return $ newEntity
            { _entColor = color & _w .~ 1
            , _entPose = newPose & posPosition .~ V3 0 20 0
            , _entSize = planeSize
            , _entShape = CubeShape
            , _entName  = "SoundBlock"
            , _entPdPatch = Just "spatula/spatula1"
            , _entCollision = Just $ \entityID _collidedWithID impulse -> when (impulse > 0.1) $ do
                pd <- view wlsPd
                
                useMaybeM_ (wldComponents . cmpPdPatch . at entityID) $ \patch -> do
                    send pd patch "note" (Atom (Float (48 + fromIntegral i * 2)))
                    send pd patch "trigger" (Atom (Float impulse))
            }

createBallMess :: MonadRandom m => m [Entity]
createBallMess = do
    -- Create a mess of planes
    let ballSize = 0.3
    forM [1..100::Int] $ \_ -> do
        color <- getRandomR (0,1)
        return $ newEntity
            { _entColor = color & _w .~ 1
            , _entPose = newPose & posPosition .~ V3 1 20 0
            , _entSize = ballSize
            , _entShape = SphereShape
            , _entName  = "MessyBall"
            }



aSpatula :: Entity
aSpatula = newEntity
        { _entSize        = V3 0.2 0.1 0.1
        , _entPose        = newPose & posPosition .~ V3 0 0.5 0
        , _entColor       = V4 0 1 1 1
        , _entPhysProps   = [IsKinematic]
        , _entShape       = CubeShape
        , _entName        = "Spatula"
        , _entUpdate      = Just $ \entityID -> do
            now <- getNow
            let a     = (*20) . sin . (/10) $ now
                spatX = (*a) . sin  $ now
                spatZ = (*a) . cos  $ now
                newPose_ = Pose (V3 spatX 0.1 spatZ) (axisAngle (V3 0 1 0) (now + (pi/2)))
            setEntityPose entityID newPose_
        }

aFloor :: Entity
aFloor = newEntity
        { _entPose = newPose & posOrientation .~ axisAngle (V3 1 0 0) ((-pi)/2)
        , _entMass = 0
        , _entShape = StaticPlaneShape
        , _entSize = V3 1000 1000 1
        , _entName = "Floor"
        }
