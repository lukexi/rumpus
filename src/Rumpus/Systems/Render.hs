{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Rumpus.Systems.Render where
import PreludeExtra

import qualified Data.Map as Map
import Rumpus.Types
import Rumpus.Systems.Shared

import TinyRick

createRenderSystem :: IO [(ShapeType, Shape Uniforms)]
createRenderSystem = do
    glEnable GL_DEPTH_TEST
    glClearColor 0 0 0.1 1

    basicProg   <- createShaderProgram "resources/shaders/default.vert" "resources/shaders/default.frag"

    cubeGeo     <- cubeGeometry (V3 1 1 1) 1
    sphereGeo   <- icosahedronGeometry 1 5 -- radius subdivisions
    planeGeo    <- planeGeometry 1 (V3 0 0 1) (V3 0 1 0) 1
    
    planeShape  <- makeShape planeGeo  basicProg
    cubeShape   <- makeShape cubeGeo   basicProg
    sphereShape <- makeShape sphereGeo basicProg

    let shapes = [(CubeShape, cubeShape), (SphereShape, sphereShape), (StaticPlaneShape, planeShape)]
    return shapes


renderSystem :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => M44 GLfloat -> m ()
renderSystem headM44 = do
    vrPal <- view wlsVRPal
    -- Render the scene
    player <- use wldPlayer
    renderWith vrPal player headM44
        (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
        (\projM44 viewM44 -> do
            renderSimulation projM44 viewM44
            renderEditors projM44 viewM44
            )


renderEditors :: (MonadState World m, MonadIO m) => M44 GLfloat -> M44 GLfloat -> m ()
renderEditors projM44 viewM44 = do
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    let projViewM44 = projM44 !*! viewM44
    editors <- Map.toList <$> use (wldComponents . cmpOnUpdateEditor)
    forM_ editors $ \(entityID, editor) -> do
        pose <- fromMaybe newPose <$> use (wldComponents . cmpPose  . at entityID)
        let codeModelM44 = transformationFromPose pose

        -- Render code in white
        renderText (editor ^. cedCodeRenderer) (projViewM44 !*! codeModelM44) (V3 1 1 1)

        let errorsModelM44 = codeModelM44 !*! identity & translation .~ V3 1 0 0

        -- Render errors in light red
        renderText (editor ^. cedErrorRenderer) (projViewM44 !*! errorsModelM44) (V3 1 0.5 0.5)

    glDisable GL_BLEND


renderSimulation :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) 
                 => M44 GLfloat -> M44 GLfloat -> m ()
renderSimulation projM44 viewM44 = do
    
    let projViewM44 = projM44 !*! viewM44

    shapes <- view wlsShapes
    forM_ shapes $ \(shapeType, shape) -> withShape shape $ do

        Uniforms{..} <- asks sUniforms
        uniformV3 uCamera (inv44 viewM44 ^. translation)

        -- Batch by entities sharing the same shape type
        entityIDsForShape <- getEntityIDsForShapeType shapeType
        forM_ entityIDsForShape $ \entityID -> do

            size  <- getEntitySize entityID
            color <- getEntityColor entityID
            pose  <- getEntityPose entityID

            let model = transformationFromPose pose !*! scaleMatrix size
            uniformM44 uModelViewProjection (projViewM44 !*! model)
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
