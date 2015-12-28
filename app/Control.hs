{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Control where
import Graphics.UI.GLFW.Pal
import Graphics.VR.Pal
import Control.Lens.Extra
import Control.Monad.State
import Control.Monad.Reader

import Entity

processControls :: (MonadIO m, MonadReader WorldStatic m, MonadState World m) => m ()
processControls = do
    VRPal{..}     <- view wlsVRPal
    -- dynamicsWorld <- view wlsDynamicsWorld
    processEvents gpEvents $ \e -> do
        closeOnEscape gpWindow e
        -- onMouseDown e $ \_ -> raycastCursorHits gpWindow dynamicsWorld projMat
        
    applyMouseLook gpWindow wldPlayer
    applyWASD gpWindow wldPlayer


-- raycastCursorHits :: (MonadIO m, MonadState World m) 
--                   => Window -> DynamicsWorld -> M44 GLfloat -> m ()
-- raycastCursorHits window dynamicsWorld projMat = do
--     playerPose <- use wldPlayer
--     cursorRay  <- cursorPosToWorldRay window projMat playerPose

--     mRayResult <- rayTestClosest dynamicsWorld cursorRay
--     forM_ mRayResult $ \rayResult -> do
--         bodyID <- getRigidBodyID (rrRigidBody rayResult)
--         mCube <- use (wldCubes . at (fromIntegral (unRigidBodyID bodyID)))
--         forM_ mCube $ \_cube -> do          
            
--             -- Convert the hit location into model space
--             -- (position, orientation) <- getBodyState (cube ^. cubBody)
--             -- let model = mkTransformation orientation position
--             --     pointOnModel = worldPointToModelPoint model (rrLocation rayResult)
--             let worldHit = rrLocation rayResult
                      
--             let cubeID = fromIntegral (unRigidBodyID bodyID)
--             [r,g,b] <- liftIO (replicateM 3 randomIO)
--             wldCubes . at cubeID . traverse . cubColor .= V4 r g b 1

--             wldCubeHits . at cubeID ?= worldHit
