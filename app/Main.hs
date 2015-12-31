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

import Render
import Entity
import Types
import Control
import Control.Monad.Reader
import Spatula

main :: IO ()
main = do
    vrPal@VRPal{..} <- initVRPal "Rumpus" [UseOpenVR]

    cubeProg  <- createShaderProgram "shaders/cube.vert" "shaders/cube.frag"
    cubeGeo   <- cubeGeometry (V3 1 1 1) 1
    cubeShape <- makeShape cubeGeo cubeProg

    useProgram (sProgram cubeShape)

    dynamicsWorld  <- createDynamicsWorld mempty
    _              <- addGroundPlane dynamicsWorld (CollisionObjectID 0) 0

    glEnable GL_DEPTH_TEST
    glClearColor 0 0 0.1 1

    let worldStatic = WorldStatic
            { _wlsDynamicsWorld = dynamicsWorld
            , _wlsCubeShape = cubeShape
            , _wlsVRPal = vrPal
            }

    void . flip runReaderT worldStatic . flip runStateT newWorld $ do 

        mapM_ createEntity =<< initScene

        whileVR vrPal $ \headM44 hands -> do
            
            collectControlEvents vrPal headM44 hands

            -- Process the update functions of each entity
            updates <- Map.toList <$> use (wldComponents . cmpUpdate)
            forM_ updates $ \(entityID, update) -> update entityID
    
            stepSimulation dynamicsWorld 90

            player <- use wldPlayer
            renderWith vrPal player headM44 
                (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
                renderSimulation
