{-# LANGUAGE OverloadedStrings #-}

module Rumpus.Main where
import Rumpus
import Halive.Recompiler
--import Rumpus.TestScene

initializeECS :: TChan CompilationRequest -> PureData -> VRPal -> ECSMonad ()
initializeECS ghc pd vrPal = do
    initAnimationSystem
    initAttachmentSystem
    initClockSystem
    initCodeEditorSystem ghc
    initCollisionsSystem
    initControlsSystem vrPal
    initCreatorSystem
    initDragSystem
    initHapticsSystem
    initKnobsSystem
    initLifetimeSystem
    initPhysicsSystem
    initPlayPauseSystem
    initProfilerSystem
    initRenderSystem
    initSynthSystem pd
    initSelectionSystem
    initSceneSystem
    initSceneWatcherSystem
    initSharedSystem
    initTextSystem

    startHandsSystem
    startKeyPadsSystem
    startSceneWatcherSystem

    loadInitialScene

loadInitialScene = (listToMaybe <$> liftIO getArgs) >>= \case
    -- Rumpus can be called like:
    -- "rumpus" which loads the Lobby,
    Nothing -> loadScene "Lobby"
    -- "rumpus Attraction" to jump straight to an existing scene,
    -- "rumpus Foo" to create a scene (when Foo is not an existing a scene)
    -- and an initial object named Foo
    Just name -> doesSceneExist name >>= \case
        True -> loadScene name
        False -> traverseM_ (createNewSceneNamed name) $ \newSceneName -> do
            loadScene newSceneName
            let fileName = name <.> "hs"
            sceneFolder <- getSceneFolder
            fileExists  <- liftIO $ doesFileExist (sceneFolder </> fileName)
            codeFile    <- if
                | fileExists    -> return (fileName, "start")
                | name == "new" -> createNewStartCodeFile -- create a new object for quick dev work
                | otherwise     -> createStartCodeFile name
            spawnEntity_ $ do
                myShape      ==> Cube
                mySize       ==> newEntitySize
                myBody       ==> Animated
                myColor      ==> V4 0.1 0.1 0.1 1
                myStartCodeFile  ==> codeFile

    --when isBeingProfiled loadTestScene


rumpusMain :: IO ()
rumpusMain = withRumpusGHC $ \ghc -> withPd $ \pd -> do
    vrPal <- initVRPal "Rumpus"

    -- Used for stress testing launch:
    --forkIO $ threadDelay 5000000 >> setWindowShouldClose (vrpWindow vrPal) True

    --singleThreadedLoop ghc pd vrPal
    multiThreadedLoop ghc pd vrPal

singleThreadedLoop :: TChan CompilationRequest -> PureData -> VRPal -> IO ()
singleThreadedLoop ghc pd vrPal = do
    void . flip runStateT newECS $ do
        initializeECS ghc pd vrPal
        whileWindow (vrpWindow vrPal) $ \windowEvents -> do
            playerM44 <- viewSystem sysControls ctsPlayer
            (headM44, events) <- tickVR vrPal playerM44 windowEvents
            profile "Controls"  $ tickControlEventsSystem headM44 events
            profile "Rendering" $ tickRenderSystem headM44

            tickLogic

tickLogic :: ECSMonad ()
tickLogic = do
    -- Perform a minor GC to just get the young objects created during the last frame
    -- without traversing all of memory
    --liftIO performMinorGC
    profile "KeyPads"           $ tickKeyPadsSystem
    profile "Clock"             $ tickClockSystem
    profile "CodeEditorInput"   $ tickCodeEditorInputSystem
    profile "CodeEditorResults" $ tickCodeEditorResultsSystem
    profile "Attachment"        $ tickAttachmentSystem
    profile "Script"            $ tickScriptSystem
    profile "Lifetime"          $ tickLifetimeSystem
    profile "Animation"         $ tickAnimationSystem
    profile "Physics"           $ tickPhysicsSystem
    profile "SyncPhysicsPoses"  $ tickSyncPhysicsPosesSystem
    profile "Collisions"        $ tickCollisionsSystem
    profile "HandControls"      $ tickHandControlsSystem
    profile "Sound"             $ tickSynthSystem
    profile "SceneWatcher"      $ tickSceneWatcherSystem

-- Experiment with running logic on the background thread.
-- Attempts to never stall the render thread,
-- (i.e. it will reuse the last world state and render it from the latest head position)
-- and has logic thread wait until a new device pose has arrived
-- from OpenVR before ticking.
multiThreadedLoop :: TChan CompilationRequest -> PureData -> VRPal -> IO ()
multiThreadedLoop ghc pd vrPal = do
    startingECS   <- execStateT (initializeECS ghc pd vrPal) newECS
    backgroundBox <- liftIO $ newTVarIO Nothing
    mainThreadBox <- liftIO $ newTVarIO startingECS

    -- LOGIC LOOP (BG THREAD)
    _ <- liftIO . forkOS $ do
        --makeContextCurrent (Just (gpThreadWindow vrPal))
        void . flip runStateT startingECS . forever $ do
            (headM44, events) <- atomically $ do
+                something <- fmap (fromJustNote "asum . repeat") $ asum $ repeat $ readTVar backgroundBox
+                writeTVar backgroundBox Nothing
+                return something

            profile "Controls" $ tickControlEventsSystem headM44 events
            tickLogic

            latestECS <- get
            atomically $ do
                writeTVar mainThreadBox $! latestECS

    -- RENDER LOOP (MAIN THREAD)
    whileWindow (vrpWindow vrPal) $ \windowEvents -> do
        latestECS         <- liftIO . atomically $ readTVar mainThreadBox
        (headM44, events) <- flip evalStateT latestECS (do
            playerM44         <- viewSystem sysControls ctsPlayer
            (headM44, events) <- tickVR vrPal playerM44 windowEvents
            -- FIXME: transforms should be calculated on background thread!
            tickRenderSystem headM44
            glFlush -- as per recommendation in openvr.h
            return (headM44, events))
        liftIO . atomically $ do
            pendingEvents <- readTVar backgroundBox >>= \case
                Just (_, pendingEvents) -> return pendingEvents
                Nothing                 -> return []
            writeTVar backgroundBox (Just (headM44, pendingEvents ++ events))
        return ()
