{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Rumpus.Main where
import PreludeExtra

import Rumpus.Types
import Rumpus.Control

import Rumpus.Systems.Attachment
import Rumpus.Systems.CodeEditor
import Rumpus.Systems.Physics
import Rumpus.Systems.Render
import Rumpus.Systems.SceneEditor
import Rumpus.Systems.Script
import Rumpus.Systems.Sound
import Halive.Utils
main :: IO ()
-- main = withPd $ \pd -> do
main = do
    pd    <- reacquire 0 $ initLibPd
    vrPal <- reacquire 1 $ initVRPal "Rumpus" [UseOpenVR]

    _               <- createSoundSystem pd
    shapes          <- createRenderSystem
    (font, ghcChan) <- createCodeEditorSystem
    dynamicsWorld   <- createPhysicsSystem

    let worldStatic = WorldStatic
            { _wlsDynamicsWorld = dynamicsWorld
            , _wlsShapes        = shapes
            , _wlsVRPal         = vrPal
            , _wlsPd            = pd
            , _wlsFont          = font
            , _wlsGHCChan       = ghcChan
            }
        world = newWorld & wldOpenALSourcePool .~ zip [1..] (pdSources pd)
                         & wldPlayer .~ if gpRoomScale vrPal == RoomScale 
                                        then newPose
                                        else newPose & posPosition .~ V3 0 1 5

    void . flip runReaderT worldStatic . flip runStateT world $ do 

        loadSceneFile "spatula/minimal.yaml"

        whileVR vrPal $ \headM44 hands -> do
            
            -- Collect control events into the events channel to be read by entities during update
            controlEventsSystem headM44 hands

            codeEditorSystem
            
            syncCodeEditorSystem

            attachmentsSystem

            use wldPlaying >>= \case
                True -> do
                    scriptingSystem
                                        
                    physicsSystem
                    
                    syncPhysicsPosesSystem

                    collisionsSystem

                    sceneEditorSystem
                False -> do
                    performDiscreteCollisionDetection dynamicsWorld

                    sceneEditorSystem

            openALSystem headM44

            renderSystem headM44




