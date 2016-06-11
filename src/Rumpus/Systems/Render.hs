-- Restricting exports to help GHC optimizer
module Rumpus.Systems.Render
    ( initRenderSystem
    , tickRenderSystem
    ) where
import PreludeExtra

import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set
import Rumpus.Systems.Shared
import Rumpus.Systems.Selection
import Rumpus.Systems.CodeEditor
import Rumpus.Systems.Controls
import Rumpus.Systems.Text
--import Rumpus.Systems.Profiler
import Graphics.GL.TextBuffer

import qualified Data.Vector.Unboxed as V

import Graphics.VR.OpenVR

data Uniforms = Uniforms
    { uProjectionView :: UniformLocation (M44 GLfloat)
    , uCamera         :: UniformLocation (V3  GLfloat)
    , uModel          :: UniformLocation (M44 GLfloat) -- only used in defaultSingle.vert
    , uColor          :: UniformLocation (V4  GLfloat) -- only used in defaultSingle.vert
    } deriving (Data)

data RenderShape = RenderShape
    { rshShapeType                 :: ShapeType
    , rshShape                     :: Shape Uniforms
    , rshStreamingArrayBuffer      :: StreamingArrayBuffer
    , rshInstanceColorsBuffer      :: ArrayBuffer (V4 GLfloat)
    , rshInstanceModelM44sBuffer   :: ArrayBuffer (M44 GLfloat)
    , rshResetShapeInstanceBuffers :: IO ()
    }

data RenderSystem = RenderSystem
    { _rdsShapes         :: ![RenderShape]
    , _rdsTextPlaneShape :: !(Shape Uniforms)
    --, _rdsWorldMatrices  :: !(Map EntityID (M44 GLfloat)) -- would like to hold these so we can query them for physics and simulation
    }
makeLenses ''RenderSystem
defineSystemKey ''RenderSystem

maxInstances :: Int
maxInstances = 2048

initRenderSystem :: (MonadIO m, MonadState ECS m) => m ()
initRenderSystem = do
    glEnable GL_DEPTH_TEST
    glClearColor 0 0 0 1

    basicProg   <- createShaderProgram "resources/shaders/default.vert" "resources/shaders/default.frag"
    singleProg  <- createShaderProgram "resources/shaders/defaultSingle.vert" "resources/shaders/default.frag"

    cubeGeo     <- cubeGeometry (V3 1 1 1) 1
    sphereGeo   <- octahedronGeometry 0.5 5 -- radius (which we halve to match boxes), subdivisions

    cubeShape   <- makeShape cubeGeo   basicProg
    sphereShape <- makeShape sphereGeo basicProg


    textPlaneGeo    <- planeGeometry 1 (V3 0 0 1) (V3 0 1 0) 1
    textPlaneShape  <- makeShape textPlaneGeo singleProg

    --let shapes = [(Cube, cubeShape), (Sphere, sphereShape), (StaticPlaneShape, planeShape)]
    let shapes = [(Cube, cubeShape), (Sphere, sphereShape)]

    shapesWithBuffers <- forM shapes $ \(shapeType, shape) -> do
        withShape shape $ do
            let streamingBufferCapacity = maxInstances * 64
            sab              <- makeSAB streamingBufferCapacity
            modelM44sBuffer  <- bufferDataEmpty GL_STREAM_DRAW streamingBufferCapacity (Proxy :: Proxy (M44 GLfloat))
            colorsBuffer     <- bufferDataEmpty GL_STREAM_DRAW streamingBufferCapacity (Proxy :: Proxy (V4  GLfloat))

            --let resetShapeInstanceBuffers = profileMS "reset" 0 $ withShape shape $ do
            let resetShapeInstanceBuffers = withShape shape $ do
                    shader <- asks sProgram
                    withArrayBuffer modelM44sBuffer $ do
                        resetSABBuffer sab modelM44sBuffer
                        assignMatrixAttributeInstanced shader "aInstanceTransform" GL_FLOAT

                    withArrayBuffer colorsBuffer $ do
                        resetSABBuffer sab colorsBuffer
                        assignFloatAttributeInstanced  shader "aInstanceColor" GL_FLOAT 4
            liftIO resetShapeInstanceBuffers

            return RenderShape
                { rshShapeType                 = shapeType
                , rshShape                     = shape
                , rshStreamingArrayBuffer      = sab
                , rshInstanceColorsBuffer      = colorsBuffer
                , rshInstanceModelM44sBuffer   = modelM44sBuffer
                , rshResetShapeInstanceBuffers = resetShapeInstanceBuffers
                }

    registerSystem sysRender (RenderSystem shapesWithBuffers textPlaneShape)


tickRenderSystem :: (MonadIO m, MonadState ECS m) => M44 GLfloat -> m ()
tickRenderSystem headM44 = do
    finalMatricesByEntityID <- getFinalMatrices
    shapeCounts             <- fillShapeBuffers finalMatricesByEntityID

    -- Render the scene
    vrPal  <- viewSystem sysControls ctsVRPal
    renderWith' vrPal headM44 $ \projM44 viewM44 -> do
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
        let projViewM44 = projM44 !*! viewM44
        renderEntities     projViewM44 shapeCounts
        renderEntitiesText projViewM44 finalMatricesByEntityID
    --putStrLnIO "Render Frame Errors:" >> glGetErrors

renderWith' :: (MonadIO m, MonadState ECS m)
            => VRPal
            -> M44 GLfloat
            -> (M44 GLfloat -> M44 GLfloat -> m b)
            -> m ()
renderWith' VRPal{..} headM44 eyeRenderFunc = do
    let viewM44 = inv44 headM44
    case gpHMD of
        NoHMD  -> do
            (x,y,w,h) <- getWindowViewport gpWindow
            glViewport x y w h
            renderFlat gpWindow viewM44 eyeRenderFunc
            swapBuffers gpWindow
        OpenVRHMD openVR -> do
            renderOpenVR' openVR viewM44 eyeRenderFunc
            when (not gpUseSDKMirror) $ do
                (w,h) <- getWindowSize gpWindow
                -- Mirror one eye to the window
                let oneEye = listToMaybe (ovrEyes openVR)
                forM_ oneEye (\eye -> mirrorOpenVREyeToWindow eye (fromIntegral w) (fromIntegral h))

                -- This is a workaround to horrible regular stalls when calling swapBuffers on the main thread.
                -- We use a one-slot MVar rather than a channel to avoid any memory leaks if the background
                -- thread can't keep up - it's not important to update the mirror window on any particular
                -- schedule as long as it happens semi-regularly.
                void . liftIO $ tryPutMVar gpBackgroundSwap (swapBuffers gpWindow)

renderOpenVR' :: (MonadIO m, MonadState ECS m)
             => OpenVR
             -> M44 GLfloat
             -> (M44 GLfloat -> M44 GLfloat -> m a)
             -> m ()
renderOpenVR' OpenVR{..} viewM44 eyeRenderFunc = do

    -- Render each eye, with multisampling
    forM_ ovrEyes $ \EyeInfo{..} -> do

        -- Will render into mfbResolveTextureID
        withMultisamplingFramebuffer eiMultisampleFramebuffer $ do
            let (x, y, w, h) = eiViewport
                finalView    = eiEyeHeadTrans !*! viewM44
            glViewport x y w h

            _ <- eyeRenderFunc eiProjection finalView
            return ()

    -- Submit frames after rendering both
    forM_ ovrEyes $ \EyeInfo{..} -> do
        let MultisampleFramebuffer{..} = eiMultisampleFramebuffer
        submitFrameForEye ovrCompositor eiEye (unTextureID mfbResolveTextureID)

fillShapeBuffers :: (MonadIO m, MonadState ECS m)
                 => Map EntityID (M44 GLfloat)
                 -> m [(Shape Uniforms, StreamingArrayBuffer, Int)]
fillShapeBuffers finalMatricesByEntityID = do
    colorsMap   <- getComponentMap myColor

    -- Batch by entities sharing the same shape type

    shapes      <- viewSystem sysRender rdsShapes
    shapeCounts <- forM shapes $ \RenderShape{..} -> withShape rshShape $ do
        --let shapeName = show rshShapeType ++ " "
        entityIDsForShape <- getEntityIDsForShapeType rshShapeType
        let count = V.length entityIDsForShape
        writeSAB rshStreamingArrayBuffer (fromIntegral count) rshResetShapeInstanceBuffers $ do
            fillSABBuffer rshInstanceColorsBuffer $ \i -> do
                let entityID = entityIDsForShape V.! i
                    color    = Map.lookupDefault 1 entityID colorsMap
                return color
            fillSABBuffer rshInstanceModelM44sBuffer $ \i -> do
                let entityID = entityIDsForShape V.! i
                    modelM44 = Map.lookupDefault identity entityID finalMatricesByEntityID
                return modelM44

        return (rshShape, rshStreamingArrayBuffer, count)
    return shapeCounts

renderEntities :: (MonadIO m, MonadState ECS m)
               => M44 GLfloat -> [(Shape Uniforms, StreamingArrayBuffer, Int)] -> m ()
renderEntities projViewM44 shapes = do

    headM44 <- getHeadPose

    forM_ shapes $ \(shape, sab, shapeCount) -> withShape shape $ do

        Uniforms{..} <- asks sUniforms
        uniformV3  uCamera (headM44 ^. translation)
        uniformM44 uProjectionView projViewM44

        drawSAB sab (fromIntegral shapeCount)


-- Perform a breadth-first traversal of entities with no parents,
-- accumulating their matrix mults all the way down into any children.
-- This avoids duplicate matrix multiplications.
getFinalMatrices :: (MonadIO m, MonadState ECS m) => m (Map EntityID (M44 GLfloat))
getFinalMatrices = do
    poseMap        <- getComponentMap myPose
    poseScaledMap  <- getComponentMap myPoseScaled
    childrenMap    <- getComponentMap myChildren
    parentMap      <- getComponentMap myParent
    --sizeMap        <- getComponentMap mySize
    inheritPoseMap <- getComponentMap myInheritPose

    let entityIDs           = Set.fromList . Map.keys $ poseMap
        entityIDsWithChild  = Set.fromList . Map.keys $ childrenMap
        entityIDsWithParent = Set.fromList . Map.keys $ parentMap
        rootIDs = Set.union entityIDs entityIDsWithChild Set.\\ entityIDsWithParent
        go mParentMatrix !accum entityID =

            let inherit                  = Map.lookup entityID inheritPoseMap
                entityMatrixLocalNoScale = Map.lookupDefault identity entityID poseMap
                entityMatrixLocal        = Map.lookupDefault identity entityID poseScaledMap

                parentTransform          = case (inherit, mParentMatrix) of
                    (Just InheritFull, Just (parentMatrix, _))        -> (parentMatrix        !*!)
                    (Just InheritPose, Just (_, parentMatrixNoScale)) -> (parentMatrixNoScale !*!)
                    _                                                 -> id

                entityMatrix        = parentTransform entityMatrixLocal
                entityMatrixNoScale = parentTransform entityMatrixLocalNoScale

                children            = Map.lookupDefault [] entityID childrenMap
            -- Pass the calculated matrix down to each child so it can calculate its own final matrix
            in foldl' (go (Just (entityMatrix, entityMatrixNoScale))) (Map.insert entityID entityMatrix accum) children
        calcMatricesForRootIDs = foldl' (go Nothing) mempty

        !finalMatricesByEntityID = calcMatricesForRootIDs rootIDs

        -- !finalMatricesByEntityID = parMapChunks 512 calcMatricesForRootIDs rootIDs
    --finalMatricesByEntityID <- naiveParMapChunks 512 calcMatricesForRootIDs rootIDs

    return finalMatricesByEntityID



getEntityIDsForShapeType :: MonadState ECS m => ShapeType -> m (V.Vector EntityID)
getEntityIDsForShapeType shapeType = V.fromList . Map.keys . Map.filter (== shapeType) <$> getComponentMap myShape


renderEntitiesText :: (MonadState ECS m, MonadIO m)
                   => M44 GLfloat -> Map EntityID (M44 GLfloat) -> m ()
renderEntitiesText projViewM44 finalMatricesByEntityID = do
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    -- Render entities with Text components
    font <- getFont
    withSharedFont font projViewM44 $ do

        entitiesWithText <- Map.toList <$> getComponentMap myTextRenderer
        forM_ entitiesWithText $ \(entityID, textRenderer) -> do
            --color <- getEntityTextColor entityID

            let parentM44 = Map.lookupDefault identity entityID finalMatricesByEntityID
            textM44 <- getEntityTextCachedM44 entityID

            renderTextPreCorrectedOfSameFont textRenderer (parentM44 !*! textM44)

    renderCodeEditors projViewM44

    glDisable GL_BLEND

renderCodeEditors :: (MonadIO m, MonadState ECS m) => M44 GLfloat -> m ()
renderCodeEditors projViewM44  = do
    -- (fixme: this is not efficient code; many state switches, shader switches, geo switches, uncached matrices)
    -- We also probably don't need gl discard in the shader if text is rendered with a background.
    glEnable GL_STENCIL_TEST
    planeShape <- viewSystem sysRender rdsTextPlaneShape
    entitiesWithStart <- Map.toList <$> getComponentMap myStartExpr
    selectedEntityID <- getSelectedEntityID
    forM_ entitiesWithStart $ \(entityID, codeExprKey) -> do
        wantsCodeHidden <- getEntityCodeHidden entityID
        let shouldDrawCode = not wantsCodeHidden || isSelectedEntityID
            isSelectedEntityID = Just entityID == selectedEntityID
        when shouldDrawCode $ do
            traverseM_ (viewSystem sysCodeEditor (cesCodeEditors . at codeExprKey)) $ \editor -> do
                parentPose       <- getEntityPose entityID
                V3 sizeX _ sizeZ <- getEntitySize entityID

                let codeModelM44 = parentPose
                        -- Offset Z by half the Z-scale to place on front of box
                        !*! translateMatrix (V3 0 0 (sizeZ/2 + 0.01))
                        -- Scale by size to fit within edges
                        !*! scaleMatrix (V3 sizeX sizeX 1)

                -- Render code in white
                headM44 <- getHeadPose
                let headPos = headM44 ^. translation
                renderTextAsScreen (editor ^. cedCodeRenderer)
                    planeShape projViewM44 codeModelM44 headPos
                    (if isSelectedEntityID then V4 0.1 0.2 0.2 1 else V4 0.0 0.0 0.1 1)

                when (textRendererHasText $ editor ^. cedErrorRenderer) $ do
                    -- Render errors in light red in panel below main
                    let errorsModelM44 = codeModelM44 !*! translateMatrix (V3 0 (-1) 0)

                    renderTextAsScreen (editor ^. cedErrorRenderer)
                        planeShape projViewM44 errorsModelM44 headPos
                        (if isSelectedEntityID then V4 0.2 0.1 0.1 1 else V4 0.1 0.0 0.0 1)
    glDisable GL_STENCIL_TEST

renderTextAsScreen :: MonadIO m => TextRenderer
                                -> Shape Uniforms
                                -> M44 GLfloat
                                -> M44 GLfloat
                                -> V3 GLfloat
                                -> V4 GLfloat
                                -> m ()
renderTextAsScreen textRenderer planeShape projViewM44 modelM44 cameraPos bgColor = do

    glStencilMask 0xFF
    glClear GL_STENCIL_BUFFER_BIT           -- Clear stencil buffer  (0 by default)

    glStencilOp GL_KEEP GL_KEEP GL_REPLACE  -- stencil-fail, depth-fail, depth-stencil-fail

    -- Draw background
    glStencilFunc GL_ALWAYS 1 0xFF          -- Set any stencil to 1
    glStencilMask 0xFF                      -- Write to stencil buffer


    withShape planeShape $ do
        Uniforms{..} <- asks sUniforms
        uniformV4  uColor bgColor
        uniformM44 uModel (modelM44 !*! translateMatrix (V3 0 0 (-0.001)))
        uniformM44 uProjectionView projViewM44
        uniformV3  uCamera cameraPos
        drawShape

    -- Draw clipped thing
    glStencilFunc GL_EQUAL 1 0xFF -- Pass test if stencil value is 1
    glStencilMask 0x00            -- Don't write anything to stencil buffer

    renderText textRenderer projViewM44 modelM44

textRendererHasText :: TextRenderer -> Bool
textRendererHasText = not . null . stringFromTextBuffer . view txrTextBuffer

