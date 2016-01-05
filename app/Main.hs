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
import Data.Maybe
import qualified Data.Map as Map

import Physics.Bullet
import Sound.Pd

import Render
import Entity
import Types
import Control
import Control.Monad.Reader
import Data.Yaml

import qualified Spatula

createRenderSystem = do
    glEnable GL_DEPTH_TEST
    glClearColor 0 0 0.1 1

    basicProg   <- createShaderProgram "spatula/cube.vert" "spatula/cube.frag"

    cubeGeo     <- cubeGeometry (V3 1 1 1) 1
    sphereGeo   <- icosahedronGeometry 1 5 -- radius subdivisions
    planeGeo    <- planeGeometry 1 (V3 0 0 1) (V3 0 1 0) 1
    
    planeShape  <- makeShape planeGeo  basicProg
    cubeShape   <- makeShape cubeGeo   basicProg
    sphereShape <- makeShape sphereGeo basicProg

    let shapes = [(CubeShape, cubeShape), (SphereShape, sphereShape), (StaticPlaneShape, planeShape)]
    return shapes

createPhysicsSystem = createDynamicsWorld mempty

main :: IO ()
main = withPd $ \pd -> do
    mapM_ (addToLibPdSearchPath pd)
        ["patches", "patches/kit", "patches/kit/list-abs"]

    vrPal  <- initVRPal "Rumpus" [UseOpenVR]

    shapes <- createRenderSystem

    dynamicsWorld <- createPhysicsSystem

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

    encodeFile "testScene.yaml" [newEntity]
    entities <- decodeFileEither "testScene.yaml"
    print (entities :: Either ParseException [Entity])

    void . flip runReaderT worldStatic . flip runStateT world $ do 

        mapM_ createEntity =<< Spatula.initScene

        whileVR vrPal $ \headM44 hands -> do
            
            -- Collect control events into the events channel to be read by entities during update
            controlEventsSystem vrPal headM44 hands

            scriptingSystem
            
            physicsSystem
            
            syncPhysicsPosesSystem

            collisionsSystem

            openALSystem headM44

            renderSystem headM44

scriptingSystem = do
    -- Process the update functions of each entity
    mapM_ (\(entityID, update) -> update entityID) 
        =<< Map.toList <$> use (wldComponents . cmpUpdate) 

renderSystem headM44 = do
    vrPal <- view wlsVRPal
    -- Render the scene
    player <- use wldPlayer
    renderWith vrPal player headM44
        (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
        renderSimulation

physicsSystem = do
    dynamicsWorld <- view wlsDynamicsWorld
    stepSimulation dynamicsWorld 90

syncPhysicsPosesSystem = do
    -- Sync rigid bodies with entity poses
    mapM_ (\(entityID, rigidBody) -> do
        pose <- uncurry Pose <$> getBodyState rigidBody
        wldComponents . cmpPose . at entityID ?= pose)
        =<< Map.toList <$> use (wldComponents . cmpRigidBody)

collisionsSystem = do
    dynamicsWorld <- view wlsDynamicsWorld
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

openALSystem headM44 = do
    -- Update souce and listener poitions
    alListenerPose (poseFromMatrix headM44)
    mapM_ (\(entityID, sourceID) -> do
        position <- view posPosition . fromMaybe newPose <$> use (wldComponents . cmpPose . at entityID)
        alSourcePosition sourceID position)
        =<< Map.toList <$> use (wldComponents . cmpSoundSource)
