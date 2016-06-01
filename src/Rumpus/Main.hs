{-# LANGUAGE OverloadedStrings #-}

module Rumpus.Main where
import Rumpus
--import Rumpus.TestScene

initializeECS ghc pd vrPal = do
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
    initSceneLoaderSystem
    initSceneWatcherSystem
    initSharedSystem
    initTextSystem

    startHandsSystem
    startKeyPadsSystem
    startSceneWatcherSystem

    listToMaybe <$> liftIO getArgs >>= \case
        Nothing -> showSceneLoader
        -- Create a fresh scene and object for quick new object dev work
        Just "new" -> do
            codeInFile <- createNewStartExpr
            rumpusRoot <- getRumpusRootFolder
            let codeFileName = takeBaseName $ fst codeInFile
                newScenePath = rumpusRoot </> codeFileName
            createdSuccessfully <- createDirectorySafe newScenePath
            when createdSuccessfully $ do
                loadScene newScenePath
                newEID <- spawnPersistentEntity $ do
                    myShape      ==> Cube
                    mySize       ==> newEntitySize
                    myProperties ==> [Floating]
                    myColor      ==> V4 0.1 0.1 0.1 1
                    myStartExpr ==> codeInFile
                sceneWatcherSaveEntity newEID
        Just sceneName -> do
            rumpusRoot <- getRumpusRootFolder
            loadScene (rumpusRoot </> sceneName)

    --when isBeingProfiled loadTestScene

rumpusMain :: IO ()
rumpusMain = withRumpusGHC $ \ghc -> withPd $ \pd -> do
    vrPal <- initVRPal "Rumpus" [UseOpenVR]

    --void . flip runStateT newECS $ do
    --    initializeECS ghc pd vrPal
    --    singleThreadedLoop vrPal
        --multiThreadedRenderInBGLoop vrPal
    multiThreadedLogicInBGLoop ghc pd vrPal

singleThreadedLoop :: VRPal -> ECSMonad ()
singleThreadedLoop vrPal = do
    whileWindow (gpWindow vrPal) $ do
        playerM44 <- viewSystem sysControls ctsPlayer
        (headM44, events) <- tickVR vrPal playerM44
        profile "Controls" $ tickControlEventsSystem headM44 events
        profile "Rendering" $ tickRenderSystem headM44

        tickLogic

tickLogic :: ECSMonad ()
tickLogic = do
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

-- Experiment with running logic on the background thread.
-- Attempts to never stall the render thread,
-- (i.e. it will reuse the last world state and render it from the latest head position)
-- and has logic thread wait until a new device pose has arrived
-- from OpenVR before ticking.
--multiThreadedLogicInBGLoop :: VRPal -> ECSMonad ()
multiThreadedLogicInBGLoop ghc pd vrPal = do
    startingECS <- execStateT (initializeECS ghc pd vrPal) newECS
    backgroundBox <- liftIO $ newTVarIO Nothing
    mainThreadBox <- liftIO $ newTVarIO startingECS

    -- LOGIC LOOP (BG THREAD)
    _ <- liftIO . forkOS $ do
        makeContextCurrent (Just (gpThreadWindow vrPal))
    --liftIO $ do
        void . flip runStateT startingECS . forever $ do
        --void . flip runStateT startingECS . replicateM_ 2 $ do
            (headM44, events) <- atomically $ do
                readTVar backgroundBox >>= \case
                    Just something -> do
                        writeTVar backgroundBox Nothing
                        return something
                    Nothing -> retry

            profile "Controls" $ tickControlEventsSystem headM44 events
            tickLogic

            latestECS <- get
            atomically $ do
                writeTVar mainThreadBox $! latestECS
    -- RENDER LOOP (MAIN THREAD)
    whileWindow (gpWindow vrPal) $ do
        latestECS <- liftIO . atomically $ readTVar mainThreadBox
        (headM44, events) <- flip evalStateT latestECS (do
            playerM44 <- viewSystem sysControls ctsPlayer
            (headM44, events) <- tickVR vrPal playerM44
            -- FIXME: transforms should be calculated on background thread!
            tickRenderSystem headM44
            glFlush -- as per recommendation in openvr.h
            return (headM44, events))
        liftIO . atomically $ do
            -- Attempt at not losing events; this can cause
            -- a memory leak though so think it through more carefully
            --pendingEvents <- readTVar backgroundBox >>= \case
            --    Just (_, pendingEvents) -> return pendingEvents
            --    Nothing -> return []
            --writeTVar backgroundBox (Just (headM44, pendingEvents ++ events))
            writeTVar backgroundBox (Just (headM44, events))
        return ()



-- Experiment with placing drawing on the background thread.
-- Doesn't render to window, probably because they are bound backwards
-- in glfw-pal (background thread should render to main window,
-- main thread should use bg window)
multiThreadedRenderInBGLoop :: VRPal -> ECSMonad ()
multiThreadedRenderInBGLoop vrPal = do
    renderChan <- liftIO newChan
    _renderWorker <- liftIO . forkOS $ do
        makeContextCurrent (Just (gpThreadWindow vrPal))
        void . forever $ do
            join (readChan renderChan)
    let onRenderThread action = do
            currentECS <- get
            liftIO $ writeChan renderChan (runStateT action currentECS)

    whileWindow (gpWindow vrPal) $ do
        playerM44 <- viewSystem sysControls ctsPlayer
        (headM44, events) <- tickVR vrPal playerM44
        profile "Controls" $ tickControlEventsSystem headM44 events
        profile "Rendering" $ onRenderThread $ tickRenderSystem headM44
        --profile "Rendering" $ tickRenderSystem headM44

        tickLogic

