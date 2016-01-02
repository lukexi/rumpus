{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Graphics.GL.Pal
import Graphics.VR.Pal

import Control.Monad
import Control.Monad.State
import Control.Lens.Extra
import qualified Data.Map as Map

import Physics.Bullet
import Sound.Pd

import Render
import Entity
import Types
import Control
import Control.Monad.Reader

import qualified Spatula

main :: IO ()
main = withPd $ \pd -> do
    mapM_ (addToLibPdSearchPath pd)
        ["patches", "patches/kit", "patches/kit/list-abs"]

    vrPal       <- initVRPal "Rumpus" [UseOpenVR]

    basicProg   <- createShaderProgram "spatula/cube.vert" "spatula/cube.frag"

    cubeGeo     <- cubeGeometry (V3 1 1 1) 1
    sphereGeo   <- icosahedronGeometry 1 5 -- radius subdivisions
    planeGeo    <- planeGeometry 1 (V3 0 0 1) (V3 0 1 0) 1
    
    planeShape  <- makeShape planeGeo  basicProg
    cubeShape   <- makeShape cubeGeo   basicProg
    sphereShape <- makeShape sphereGeo basicProg

    dynamicsWorld <- createDynamicsWorld mempty

    let shapes = [(CubeShape, cubeShape), (SphereShape, sphereShape), (StaticPlaneShape, planeShape)]

    glEnable GL_DEPTH_TEST
    glClearColor 0 0 0.1 1

    let worldStatic = WorldStatic
            { _wlsDynamicsWorld = dynamicsWorld
            , _wlsShapes        = shapes
            , _wlsVRPal         = vrPal
            , _wlsPd            = pd
            }
        world = newWorld & wldOpenALSourcePool .~ zip [1..] (pdSources pd)
                         & wldPlayer .~ if gpRoomScale vrPal == RoomScale 
                                        then newPose 
                                        else newPose & posPosition .~ V3 0 1 5 

    void . flip runReaderT worldStatic . flip runStateT world $ do 

        mapM_ createEntity =<< Spatula.initScene

        whileVR vrPal $ \headM44 hands -> do
            
            -- Collect control events into the events channel to be read by entities during update
            collectControlEvents vrPal headM44 hands

            -- Process the update functions of each entity
            mapM_ (\(entityID, update) -> update entityID) 
                =<< Map.toList <$> use (wldComponents . cmpUpdate) 
            
            -- Step the physics simulation
            stepSimulation dynamicsWorld 90

            -- Tell objects about any collisions
            collisions <- getCollisions dynamicsWorld
            
            forM_ collisions $ \collision -> do
                let bodyAID = (fromIntegral . unCollisionObjectID . cbBodyAID) collision
                    bodyBID = (fromIntegral . unCollisionObjectID . cbBodyBID) collision
                    appliedImpulse = cbAppliedImpulse collision
                mapM_ (\onCollision -> onCollision bodyAID bodyBID appliedImpulse)
                    =<< use (wldComponents . cmpCollision . at bodyAID)
                mapM_ (\onCollision -> onCollision bodyBID bodyAID appliedImpulse)
                    =<< use (wldComponents . cmpCollision . at bodyBID)

            -- Update souce and listener poitions
            alListenerPose (poseFromMatrix headM44)
            mapM_ (\(entityID, sourceID) -> alSourcePosition sourceID =<< view posPosition <$> getEntityPose entityID)
                =<< Map.toList <$> use (wldComponents . cmpSoundSource)

            -- Render the scene
            player <- use wldPlayer
            renderWith vrPal player headM44 
                (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
                renderSimulation
