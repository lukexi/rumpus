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

    void . flip runReaderT worldStatic . flip runStateT newWorld $ do 

        mapM_ createEntity =<< Spatula.initScene

        whileVR vrPal $ \headM44 hands -> do
            
            collectControlEvents vrPal headM44 hands

            -- Process the update functions of each entity
            mapM_ (\(entityID, update) -> update entityID) 
                =<< Map.toList <$> use (wldComponents . cmpUpdate) 
            
            stepSimulation dynamicsWorld 90

            player <- use wldPlayer
            renderWith vrPal player headM44 
                (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
                renderSimulation
