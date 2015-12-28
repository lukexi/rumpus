{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Render where
import Graphics.GL.Pal
import Control.Lens.Extra
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Physics.Bullet
import Entity
import qualified Data.Map as Map

renderSimulation :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) 
                 => M44 GLfloat -> M44 GLfloat -> m ()
renderSimulation projMat viewMat = do
    cubeShape <- view wlsCubeShape
    let Uniforms{..} = sUniforms cubeShape
    
    uniformV3 uCamera =<< use (wldPlayer . posPosition)

    let viewProj = projMat !*! viewMat

    withVAO (sVAO cubeShape) $ do
        entities <- Map.toList <$> use wldEntities
        
        forM_ entities $ \(_entityID, entity) -> do

            modelMatrix <- transformationFromPose <$> case entity ^. entRigidBody of
                Just rigidBody -> uncurry Pose <$> getBodyState rigidBody
                Nothing        -> return (entity ^. entPose)
            
            -- mCubeHit <- use (wldCubeHits . at cubeID)
            -- forM_ mCubeHit $ \cubeHit ->
            --     uniformV3 uCubeHit cubeHit

            let model = modelMatrix !*! scaleMatrix (entity ^. entSize)
            uniformM44 uModelViewProjection (viewProj !*! model)
            uniformM44 uInverseModel        (inv44 model)
            uniformM44 uModel               model
            uniformV4  uDiffuse             (entity ^. entColor)

            glDrawElements GL_TRIANGLES (geoVertCount (sGeometry cubeShape)) GL_UNSIGNED_INT nullPtr
