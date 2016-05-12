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

            tickKeyPadsSystem
            tickClockSystem
            tickCodeEditorInputSystem
            tickCodeEditorResultsSystem
            tickAttachmentSystem
            tickConstraintSystem
            tickScriptSystem
            tickLifetimeSystem
            tickAnimationSystem
            tickPhysicsSystem
            tickSyncPhysicsPosesSystem
            tickCollisionsSystem
            tickHandControlsSystem
            tickSoundSystem
