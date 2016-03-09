{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Rumpus.Main where
import PreludeExtra

import Rumpus.Systems.Animation
import Rumpus.Systems.Attachment
import Rumpus.Systems.CodeEditor
import Rumpus.Systems.Collisions
import Rumpus.Systems.Constraint
import Rumpus.Systems.Controls
import Rumpus.Systems.Hands
import Rumpus.Systems.Lifetime
import Rumpus.Systems.Physics
import Rumpus.Systems.Render
import Rumpus.Systems.SceneEditor
import Rumpus.Systems.Script
import Rumpus.Systems.Selection
import Rumpus.Systems.Shared
import Rumpus.Systems.Sound
import Rumpus.Systems.PlayPause

import Halive.Utils

main :: IO ()
main = withPd $ \pd -> do
-- main = do
    vrPal <- reacquire 0 $ initVRPal "Rumpus" [UseOpenVR]
    -- pd    <- reacquire 1 $ initLibPd
    
    args <- getArgs
    -- let sceneName = fromMaybe "default-scene" (listToMaybe args)
    let sceneName = listToMaybe args

    void . flip runStateT newECS $ do 

        initAnimationSystem
        initAttachmentSystem
        initCodeEditorSystem
        initCollisionsSystem
        initConstraintSystem
        initControlsSystem vrPal
        initLifetimeSystem
        initPhysicsSystem
        initPlayPauseSystem
        initRenderSystem
        initSceneEditorSystem
        initScriptSystem
        initSoundSystem pd
        initSelectionSystem
        initSharedSystem

        startHandsSystem
        forM_ sceneName loadScene
        -- loadScene sceneName
        
        -- testEntity <- spawnEntity Transient $ return ()
        -- addCodeExpr testEntity "CollisionStart" "collisionStart" cmpOnCollisionStartExpr cmpOnCollisionStart        
        -- selectEntity testEntity
        -- _ <- spawnEntity Transient $ do
        --     cmpOnStart ==> start
        --     cmpPhysicsProperties ==> [IsKinematic, NoContactResponse]
        --     return ()

        whileVR vrPal $ \headM44 hands vrEvents -> do
        
            tickControlEventsSystem headM44 hands vrEvents
            tickCodeEditorSystem
            tickSyncCodeEditorSystem
            tickAttachmentSystem
            tickConstraintSystem
            tickScriptSystem
            tickLifetimeSystem
            tickAnimationSystem
            tickPhysicsSystem
            tickSyncPhysicsPosesSystem
            tickCollisionsSystem
            tickSceneEditorSystem
            tickSoundSystem headM44
            tickRenderSystem headM44
        -- whileVR vrPal $ \headM44 hands vrEvents -> profile "frame" 0 $ do
            
            -- profile "tickControlEventsSystem" 1 $ tickControlEventsSystem headM44 hands vrEvents
            -- profile "tickCodeEditorSystem" 1 $ tickCodeEditorSystem
            -- profile "tickSyncCodeEditorSystem" 1 $ tickSyncCodeEditorSystem
            -- profile "tickAttachmentSystem" 1 $ tickAttachmentSystem
            -- profile "tickConstraintSystem" 1 $ tickConstraintSystem
            -- profile "tickScriptSystem" 1 $ tickScriptSystem
            -- profile "tickLifetimeSystem" 1 $ tickLifetimeSystem
            -- profile "tickAnimationSystem" 1 $ tickAnimationSystem
            -- profile "tickPhysicsSystem" 1 $ tickPhysicsSystem
            -- profile "tickSyncPhysicsPosesSystem" 1 $ tickSyncPhysicsPosesSystem
            -- profile "tickCollisionsSystem" 1 $ tickCollisionsSystem
            -- profile "tickSceneEditorSystem" 1 $ tickSceneEditorSystem
            -- profile "tickSoundSystem" 1 $ tickSoundSystem headM44
            -- profile "tickRenderSystem" 1 $ tickRenderSystem headM44



