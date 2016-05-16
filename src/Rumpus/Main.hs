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
        initSharedSystem
        initTextSystem

        startHandsSystem
        startKeyPadsSystem
        startSceneSystem
        when isBeingProfiled loadTestScene

        whileWindow (gpWindow vrPal) $ do
            playerM44 <- viewSystem sysControls ctsPlayer
            (headM44, events) <- tickVR vrPal playerM44
            tickControlEventsSystem headM44 events
            tickRenderSystem headM44

            -- Perform a minor GC to just get the young objects created during the last frame
            -- without traversing all of memory
            --liftIO performMinorGC

            profile "tickKeyPadsSystem" $ tickKeyPadsSystem
            profile "tickClockSystem" $ tickClockSystem
            profile "tickCodeEditorInputSystem" $ tickCodeEditorInputSystem
            profile "tickCodeEditorResultsSystem" $ tickCodeEditorResultsSystem
            profile "tickAttachmentSystem" $ tickAttachmentSystem
            profile "tickConstraintSystem" $ tickConstraintSystem
            profile "tickScriptSystem" $ tickScriptSystem
            profile "tickLifetimeSystem" $ tickLifetimeSystem
            profile "tickAnimationSystem" $ tickAnimationSystem
            profile "tickPhysicsSystem" $ tickPhysicsSystem
            profile "tickSyncPhysicsPosesSystem" $ tickSyncPhysicsPosesSystem
            profile "tickCollisionsSystem" $ tickCollisionsSystem
            profile "tickHandControlsSystem" $ tickHandControlsSystem
            profile "tickSoundSystem" $ tickSoundSystem
