{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Render where
import Graphics.GL.Pal
import Control.Lens.Extra
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Maybe

import Types

renderSimulation :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) 
                 => M44 GLfloat -> M44 GLfloat -> m ()
renderSimulation projMat viewMat = do
    
    let viewProj = projMat !*! viewMat

    shapes <- view wlsShapes
    forM_ shapes $ \(shapeType, shape) -> withShape shape $ do

        Uniforms{..} <- asks sUniforms
        uniformV3 uCamera (inv44 viewMat ^. translation)

        -- Batch by entities sharing the same shape type
        entityIDsForShape <- getEntityIDsForShapeType shapeType
        forM_ entityIDsForShape $ \entityID -> do

            
            size       <- fromMaybe 1 <$> use (wldComponents . cmpSize . at entityID)
            color      <- fromMaybe 1 <$> use (wldComponents . cmpColor . at entityID)
            pose       <- fromMaybe newPose <$> use (wldComponents . cmpPose . at entityID)

            let model = transformationFromPose pose !*! scaleMatrix size
            uniformM44 uModelViewProjection (viewProj !*! model)
            uniformM44 uInverseModel        (inv44 model)
            uniformM44 uModel               model
            uniformV4  uDiffuse             color

            drawShape

-- | Accumulate a matrix stack by walking up to the parent
getEntityTotalModelMatrix :: MonadState World m => EntityID -> m (M44 GLfloat)
getEntityTotalModelMatrix startEntityID = do
    
    let go Nothing = return identity
        go (Just entityID) = do
            pose   <- fromMaybe newPose <$> use (wldComponents . cmpPose . at entityID)
            parent <- use (wldComponents . cmpParent . at entityID)
            (transformationFromPose pose !*!) <$> go parent
    
    go (Just startEntityID)

getEntityIDsForShapeType :: MonadState World f => ShapeType -> f [EntityID]
getEntityIDsForShapeType shapeType = Map.keys . Map.filter (== shapeType) <$> use (wldComponents . cmpShape)
