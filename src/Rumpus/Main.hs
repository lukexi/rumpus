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
{-
        renderChan <- liftIO newChan
        renderWorker <- liftIO . forkOS $ do
            makeContextCurrent (Just (gpThreadWindow vrPal))
            void . forever $ do
                join (readChan renderChan)
        let onRenderThread action = do
                ecs <- get
                liftIO $ writeChan renderChan (runStateT action ecs)

        whileWindow (gpWindow vrPal) $ do
            playerM44 <- viewSystem sysControls ctsPlayer
            (headM44, events) <- tickVR vrPal playerM44
            profile "Controls" $ tickControlEventsSystem headM44 events
            profile "Rendering" $ onRenderThread $ tickRenderSystem headM44
            --profile "Rendering" $ tickRenderSystem headM44

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
-}



        startingECS <- get
        backgroundBox <- liftIO $ newTVarIO Nothing
        mainThreadBox <- liftIO $ newTVarIO startingECS
        liftIO . forkOS $ do
            makeContextCurrent (Just (gpThreadWindow vrPal))
            void . flip runStateT startingECS . forever $ do
                (headM44, events) <- atomically $ do
                    readTVar backgroundBox >>= \case
                        Just something -> do
                            writeTVar backgroundBox Nothing
                            return something
                        Nothing -> retry

                profile "Controls" $ tickControlEventsSystem headM44 events
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

                newECS <- get
                atomically $ do
                    writeTVar mainThreadBox newECS


        whileWindow (gpWindow vrPal) $ do
            ecs <- liftIO . atomically $ readTVar mainThreadBox
            (headM44, events) <- flip evalStateT ecs (do
                playerM44 <- viewSystem sysControls ctsPlayer
                (headM44, events) <- tickVR vrPal playerM44
                profile "Rendering" $ tickRenderSystem headM44
                return (headM44, events))
            liftIO . atomically $ do
                pendingEvents <- readTVar backgroundBox >>= \case
                    Just (_, pendingEvents) -> return pendingEvents
                    Nothing -> return []
                writeTVar backgroundBox (Just (headM44, pendingEvents ++ events))

