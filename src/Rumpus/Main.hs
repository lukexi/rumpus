{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Rumpus.Main where
import PreludeExtra

import Rumpus.Types
import Rumpus.Control
import Rumpus.ECS

import Rumpus.Systems.Attachment
import Rumpus.Systems.Animation
import Rumpus.Systems.CodeEditor
import Rumpus.Systems.Lifetime
import Rumpus.Systems.Physics
import Rumpus.Systems.Render
import Rumpus.Systems.SceneEditor
import Rumpus.Systems.Script
import Rumpus.Systems.Sound
import Rumpus.Systems.Constraint
import Halive.Utils

main :: IO ()
main = withPd $ \pd -> do

-- main = do
    vrPal <- reacquire 0 $ initVRPal "Rumpus" [UseOpenVR]
    -- pd    <- reacquire 1 $ initLibPd

    _               <- createSoundSystem pd
    shapes          <- createRenderSystem
    (font, ghcChan) <- createCodeEditorSystem
    dynamicsWorld   <- createPhysicsSystem

    args <- getArgs
    let sceneName = fromMaybe "minimal" (listToMaybe args)

    void . flip runStateT newWorld $ do 

        -- loadScene sceneName

        whileVR vrPal $ \headM44 hands vrEvents -> do
            
            -- Collect control events into the events channel to be read by entities during update
            controlEventsSystem headM44 hands vrEvents

            codeEditorSystem
            
            syncCodeEditorSystem

            attachmentsSystem

            constraintSystem

            isPlaying <- viewSystem controlSystemKey ctsPlaying
            if isPlaying
                then do
                    scriptingSystem

                    lifetimeSystem
                    
                    animationSystem
                                        
                    physicsSystem
                    
                    syncPhysicsPosesSystem

                    collisionsSystem
                else do
                    performDiscreteCollisionDetection dynamicsWorld

            sceneEditorSystem

            soundSystem headM44

            renderSystem headM44




