{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Rumpus.Main where
import Rumpus
import Rumpus.TestScene
import Rumpus.CopyScenes
import Halive.Utils
import Halive.Recompiler

rumpusMain :: IO ()
rumpusMain = withGHC rumpusGHCSessionConfig $ \ghc -> withPd $ \pd -> do
-- rumpusMain = do
    vrPal <- reacquire 0 $ initVRPal "Rumpus" [UseOpenVR]
    --vrPal <- reacquire 0 $ initVRPal "Rumpus" []
    -- pd    <- reacquire 1 $ initLibPd

    scene <- fromMaybe "Room" . listToMaybe <$> getArgs
    userSceneFolder <- copyStartScene scene

    void . flip runStateT newECS $ do

        initAnimationSystem
        initAttachmentSystem
        initClockSystem
        initCodeEditorSystem ghc
        initCollisionsSystem
        initConstraintSystem
        initControlsSystem vrPal
        initDragSystem
        initHapticsSystem
        initLifetimeSystem
        initPhysicsSystem
        initPlayPauseSystem
        initRenderSystem
        initSceneEditorSystem
        initSoundSystem pd
        initSelectionSystem
        initSceneSystem userSceneFolder
        initSharedSystem
        initTextSystem

        startHandsSystem
        startKeyPadsSystem

        -- Profiling doesn't support hot code load, so load the test scene instead
        let useTestScene = isBeingProfiled
        if
            | isInReleaseMode  -> loadScene userSceneFolder
            | not useTestScene -> loadScene (pristineSceneDirWithName scene)
            | otherwise        -> loadTestScene

        whileWindow (gpWindow vrPal) $ do
            playerM44 <- viewSystem sysControls ctsPlayer
            (headM44, events) <- tickVR vrPal playerM44
            --(headM44, events) <- tickVR vrPal (playerM44 !*! scaleMatrix 5)
            --(headM44, events) <- tickVR vrPal (playerM44 !*! scaleMatrix 0.1)
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


            --checkFPS


--profileMS' "gc"          1 $
--profileMS' "keyhands"    1 $
--profileMS' "codeinput"   1 $
--profileMS' "codeupdate"  1 $
--profileMS' "attachments" 1 $
--profileMS' "constraints" 1 $
--profileMS' "script"      1 $
--profileMS' "lifetime"    1 $
--profileMS' "animation"   1 $
--profileMS' "physicsRun"  1 $
--profileMS' "physicsCopy" 1 $
--profileMS' "collisions"  1 $
--profileMS' "sceneEditor" 1 $
--profileMS' "sound"       1 $
--profileMS' "controls"    1 $
--profileMS' "render"      1 $

makeCheckFPS :: MonadIO m => m (m ())
makeCheckFPS = do
    fpsRef <- liftIO . newIORef =<< liftIO getCurrentTime
    let checkFPS = liftIO $ do
            now  <- getCurrentTime
            last <- readIORef fpsRef
            writeIORef fpsRef now
            let timeDiff = now `diffUTCTime` last
            putStrLn ("FPS: " ++ show (1/timeDiff))
    return checkFPS
