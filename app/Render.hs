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
import Data.Maybe

renderSimulation :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) 
                 => M44 GLfloat -> M44 GLfloat -> m ()
renderSimulation projMat viewMat = do
    cubeShape <- view wlsCubeShape
    let Uniforms{..} = sUniforms cubeShape
    
    uniformV3 uCamera =<< use (wldPlayer . posPosition)

    let viewProj = projMat !*! viewMat

    shapes <- Map.toList <$> use (wldComponents . cmpShape)
    withVAO (sVAO cubeShape) $ do
        
        forM_ shapes $ \(entityID, _shape) -> do

            mRigidBody <- use (wldComponents . cmpRigidBody . at entityID)
            pose <- fromMaybe newPose <$> use (wldComponents . cmpPose . at entityID)
            size <- fromMaybe 1 <$> use (wldComponents . cmpSize . at entityID)
            color <- fromMaybe 1 <$> use (wldComponents . cmpColor . at entityID)

            modelMatrix <- transformationFromPose <$> case mRigidBody of
                Just rigidBody -> uncurry Pose <$> getBodyState rigidBody
                Nothing        -> return pose
            
            -- mCubeHit <- use (wldCubeHits . at cubeID)
            -- forM_ mCubeHit $ \cubeHit ->
            --     uniformV3 uCubeHit cubeHit

            let model = modelMatrix !*! scaleMatrix size
            uniformM44 uModelViewProjection (viewProj !*! model)
            uniformM44 uInverseModel        (inv44 model)
            uniformM44 uModel               model
            uniformV4  uDiffuse             color

            glDrawElements GL_TRIANGLES (geoVertCount (sGeometry cubeShape)) GL_UNSIGNED_INT nullPtr
