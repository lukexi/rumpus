{-# LANGUAGE OverloadedStrings #-}

module Rumpus.Main where
import Rumpus
import Rumpus.TestScene

rumpusMain :: IO ()
rumpusMain = withRumpusGHC $ \ghc -> withPd $ \pd -> do
    vrPal <- initVRPal "Rumpus" [UseOpenVR]

    void . flip runStateT newECS $ do

        initAnimationSystem
        initAttachmentSystem
        initClockSystem
        initCodeEditorSystem ghc
        initCollisionsSystem
        initConstraintSystem
        initControlsSystem vrPal
        initCreatorSystem
        initDragSystem
        initHapticsSystem
        initLifetimeSystem
        initPhysicsSystem
        initPlayPauseSystem
        initProfilerSystem
        initRenderSystem
        initSoundSystem pd
        initSelectionSystem
        initSceneSystem
        initSceneWatcherSystem
        initSharedSystem
        initTextSystem

        startHandsSystem
        startKeyPadsSystem
        startSceneSystem
        startSceneWatcherSystem
        when isBeingProfiled loadTestScene

        whileWindow (gpWindow vrPal) $ do
            playerM44 <- viewSystem sysControls ctsPlayer
            (headM44, events) <- tickVR vrPal playerM44
            profile "tickControlEventsSystem" $ tickControlEventsSystem headM44 events
            profile "tickRenderSystem" $ tickRenderSystem headM44

            -- Perform a minor GC to just get the young objects created during the last frame
            -- without traversing all of memory
            --liftIO performMinorGC

            profile "KeyPads" $ tickKeyPadsSystem
            profile "Clock" $ tickClockSystem
            profile "CodeEditorInput" $ tickCodeEditorInputSystem
            profile "CodeEditorResults" $ tickCodeEditorResultsSystem
            profile "Attachment" $ tickAttachmentSystem
            profile "Constraint" $ tickConstraintSystem
            profile "Script" $ tickScriptSystem
            profile "Lifetime" $ tickLifetimeSystem
            profile "Animation" $ tickAnimationSystem
            profile "Physics" $ tickPhysicsSystem
            profile "SyncPhysicsPoses" $ tickSyncPhysicsPosesSystem
            profile "Collisions" $ tickCollisionsSystem
            profile "HandControls" $ tickHandControlsSystem
            profile "Sound" $ tickSoundSystem
            profile "SceneWatcher" $ tickSceneWatcherSystem
