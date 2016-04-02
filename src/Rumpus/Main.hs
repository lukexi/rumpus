{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Rumpus.Main where
import Rumpus
import Rumpus.TestScene
import Halive.Utils

-- | Copy the 'pristine' Scenes folder into the user's Documents/Rumpus directory on startup
-- if the Documents/Rumpus folder is missing.  
copyScenes :: IO FilePath
copyScenes = do
    userDocsDir <- getUserDocumentsDirectory
    let userRoomDir    = userDocsDir </> "Rumpus" </> "Scenes" </> "Room"
        pristineScenes = "pristine" </> "Scenes" </> "Room"
    exists  <- doesDirectoryExist userRoomDir
    when (not exists) $ do
        createDirectoryIfMissing True userRoomDir
        roomFiles <- filter (not . (`elem` [".", ".."])) <$> getDirectoryContents pristineScenes

        forM_ roomFiles $ \roomFile -> 
            copyFile (pristineScenes </> roomFile) (userRoomDir </> roomFile)
    -- When not in release mode, we want to edit the pristine folder directly
    -- so we can track changes in git.
    return $ if isInReleaseMode then userRoomDir else pristineScenes


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

        -- XXXXXXXXXXXXXXXX Turning off scene loading in dev mode while testing profiled renderer.
        when isInReleaseMode $ loadScene sceneFolder

        unless isInReleaseMode loadTestScene
        
        
        whileVR vrPal $ \headM44 hands vrEvents -> profileFPS' "frame" 0 $ do

            -- Perform a minor GC to just get the young objects created during the last frame
            -- without traversing all of memory
            profileMS' "gc"          1 $ liftIO performMinorGC
            
            profileMS' "controls"    1 $ tickControlEventsSystem headM44 hands vrEvents
            profileMS' "codeinput"   1 $ tickCodeEditorInputSystem
            profileMS' "codeupdate"  1 $ tickCodeEditorResultsSystem
            profileMS' "attachments" 1 $ tickAttachmentSystem
            profileMS' "constraints" 1 $ tickConstraintSystem
            profileMS' "script"      1 $ tickScriptSystem
            profileMS' "lifetime"    1 $ tickLifetimeSystem
            profileMS' "animation"   1 $ tickAnimationSystem
            profileMS' "physicsRun"  1 $ tickPhysicsSystem
            profileMS' "physicsCopy" 1 $ tickSyncPhysicsPosesSystem
            profileMS' "collisions"  1 $ tickCollisionsSystem
            profileMS' "sceneEditor" 1 $ tickSceneEditorSystem
            profileMS' "sound"       1 $ tickSoundSystem headM44
            profileMS' "render"      1 $ tickRenderSystem headM44


