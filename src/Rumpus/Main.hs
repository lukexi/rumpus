{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Rumpus.Main where
import Rumpus
import Rumpus.TestScene
import Rumpus.CopyScenes
import Halive.Utils

rumpusMain :: IO ()
rumpusMain = withPd $ \pd -> do
-- rumpusMain = do
    vrPal <- reacquire 0 $ initVRPal "Rumpus" [UseOpenVR]
    --vrPal <- reacquire 0 $ initVRPal "Rumpus" []
    -- pd    <- reacquire 1 $ initLibPd
    
    sceneFolder <- copyScenes
    --args <- getArgs
    --let sceneName = fromMaybe "room" $ listToMaybe args

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
        initSoundSystem pd
        initSelectionSystem sceneFolder
        initSharedSystem
        initTextSystem

        startHandsSystem
        startKeyboardHandsSystem

        let useTestScene = False
        if 
            | isInReleaseMode  -> loadScene sceneFolder
            | not useTestScene -> loadScene sceneFolder
            | otherwise        -> loadTestScene
        


        --fpsRef <- liftIO . newIORef =<< liftIO getCurrentTime
        --let checkFPS = liftIO $ do
        --        now  <- getCurrentTime
        --        last <- readIORef fpsRef
        --        writeIORef fpsRef now
        --        let timeDiff = now `diffUTCTime` last
        --        putStrLn ("FPS: " ++ show (1/timeDiff))


        whileWindow (gpWindow vrPal) $ do
            playerM44 <- viewSystem sysControls ctsPlayer
            (headM44, vrEvents) <- tickVR vrPal playerM44


            -- Perform a minor GC to just get the young objects created during the last frame
            -- without traversing all of memory
            liftIO performMinorGC
            
            tickKeyboardHandsSystem
            tickControlEventsSystem headM44 vrEvents
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
            tickSceneEditorSystem
            tickSoundSystem headM44
            tickRenderSystem headM44
            --checkFPS


--profileMS' "gc"          1 $ 
--profileMS' "keyhands"    1 $ 
--profileMS' "controls"    1 $ 
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
--profileMS' "render"      1 $ 