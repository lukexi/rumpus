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
import Rumpus.Systems.PlayPause
import Rumpus.Systems.Render
import Rumpus.Systems.SceneEditor
import Rumpus.Systems.Script
import Rumpus.Systems.Selection
import Rumpus.Systems.Shared
import Rumpus.Systems.Sound
import Rumpus.Systems.Text
import Rumpus.Types

import Halive.Utils


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
        initScriptSystem
        initSoundSystem pd
        initSelectionSystem sceneFolder
        initSharedSystem
        initTextSystem

        startHandsSystem
        loadScene sceneFolder
        
        -- testEntity <- spawnEntity Transient $ return ()
        -- addCodeExpr testEntity "CollisionStart" "collisionStart" cmpOnCollisionStartExpr cmpOnCollisionStart        
        -- selectEntity testEntity

        --_ <- spawnEntity Transient $ do
        --    cmpOnStart ==> testBuildTreeStart
        --    cmpPhysicsProperties ==> [IsKinematic]
        --    return ()
        
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

profileMS' :: (MonadIO m) => String -> Int -> m a -> m a
--profileMS' = profileMS
profileMS' _ _ = id
--profileMS' name _ act = putStrLnIO ("About to run " ++ name ++ "...") >> act 

profileFPS' :: (MonadIO m) => String -> Int -> m a -> m a
--profileFPS' = profileFPS
profileFPS' _ _ = id


testBuildTreeStart :: OnStart
testBuildTreeStart = do
    removeChildren
    
    let branch parentID n pos = do
            childID <- spawnEntity Transient $ do
                cmpParent ==> parentID
                cmpPose   ==> mkTransformation (axisAngle (V3 0 0 1) 0.3) pos
                cmpShapeType              ==> CubeShape
                cmpPhysicsProperties      ==> [NoPhysicsShape]
                cmpInheritParentTransform ==> InheritFull
                cmpSize                   ==> V3 0.5 0.6 0.6
                cmpColor ==> hslColor (fromIntegral n/9) 0.8 0.5
                cmpOnUpdate ==> do
                    now <- sin <$> getNow
                    cmpPose ==> mkTransformation (axisAngle (V3 0 1 1) now) pos
            when (n > 0) $ do
                branch childID (n - 1) (V3 1 1 0)
                branch childID (n - 1) (V3 (-1) 1 0)
    rootEntityID <- ask
    branch rootEntityID (5::Int) 0
    return Nothing
