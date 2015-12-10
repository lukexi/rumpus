{-# LANGUAGE FlexibleContexts, LambdaCase, RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.VR.Pal

import Control.Monad
import Control.Monad.State
import Control.Lens.Extra
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random

import Physics.Bullet


type ObjectID = Int

data Cube = Cube
  { _cubColor :: !(V4 GLfloat)
  , _cubBody  :: !RigidBody
  , _cubSize  :: !(V3 GLfloat)
  }
makeLenses ''Cube


data Uniforms = Uniforms
    { uModelViewProjection :: UniformLocation (M44 GLfloat)
    , uInverseModel        :: UniformLocation (M44 GLfloat)
    , uModel               :: UniformLocation (M44 GLfloat)
    , uCamera              :: UniformLocation (V3  GLfloat)
    , uDiffuse             :: UniformLocation (V4  GLfloat)
    , uCubeHit             :: UniformLocation (V3  GLfloat)
    } deriving (Data)

data World = World
    { _wldPlayer    :: !(Pose GLfloat)
    , _wldCubes     :: !(Map ObjectID Cube)
    , _wldCubeHits  :: !(Map ObjectID (V3 GLfloat))
    }
makeLenses ''World

spatulaID = 99999

newWorld :: World
newWorld = World
    (Pose (V3 0 20 60) (axisAngle (V3 0 1 0) 0))
    mempty
    mempty

createSpatula :: (MonadIO m, MonadState World m) => DynamicsWorld -> m RigidBody
createSpatula dynamicsWorld = do
    let spatulaSize = V3 2 1 1
    rigidBody <- addCube dynamicsWorld (RigidBodyID spatulaID) 
        mempty { pcPosition = V3 0 0.5 0
               , pcRotation = axisAngle (V3 0 1 0) 0 
               , pcScale = spatulaSize
               }
    setRigidBodyKinematic rigidBody
    -- Create a spatula to stir them up
    wldCubes . at (fromIntegral spatulaID) ?= Cube
        { _cubBody  = rigidBody
        , _cubColor = V4 0 1 1 1
        , _cubSize  = spatulaSize
        }
    return rigidBody



main :: IO ()
main = do
    
    let fov = 45

    VRPal{..} <- initVRPal "Bullet" []

    cubeProg  <- createShaderProgram "shaders/cube.vert" "shaders/cube.frag"
    cubeGeo   <- cubeGeometry (V3 1 1 1) 1
    cubeShape <- makeShape cubeGeo cubeProg

    useProgram (sProgram cubeShape)

    dynamicsWorld  <- createDynamicsWorld mempty
    _              <- addGroundPlane dynamicsWorld (RigidBodyID 0) 0

    glEnable GL_DEPTH_TEST

    glClearColor 0 0 0.1 1


    void . flip runStateT newWorld $ do 
        -- Create a mess of planes
        let planeSize = V3 1 1 0.1
        forM_ [1..1000] $ \i -> do
            rigidBody <- addCube dynamicsWorld (RigidBodyID i) mempty 
                { pcPosition = V3 0 20 0
                , pcRotation = axisAngle (V3 0 1 0) 0
                , pcScale = planeSize
                }
            [r,g,b] <- liftIO (replicateM 3 randomIO)
            wldCubes . at (fromIntegral i) ?= Cube
                { _cubBody = rigidBody
                , _cubColor = V4 r g b 1
                , _cubSize = planeSize
                }

        spatulaRigidBody <- createSpatula dynamicsWorld

        whileWindow gpWindow $ do
            now <- getNow
            let a     = (*20) . sin . (/10) $ now
                spatX = (*a) . sin  $ now
                spatZ = (*a) . cos  $ now
                _ = spatX :: Double
            setRigidBodyWorldTransform spatulaRigidBody (V3 spatX 0.4 spatZ) (axisAngle (V3 0 1 0) (now + (pi/2)))

            projMat <- getWindowProjection gpWindow fov 0.1 1000
            viewMat <- viewMatrixFromPose <$> use wldPlayer
            (x,y,w,h) <- getWindowViewport gpWindow
            glViewport x y w h
            
            processEvents gpEvents $ \e -> do
                closeOnEscape gpWindow e
                onMouseDown e $ \_ -> raycastCursorHits gpWindow dynamicsWorld projMat
            
            applyMouseLook gpWindow wldPlayer
            applyWASD gpWindow wldPlayer        
    
            stepSimulation dynamicsWorld

            renderSimulation gpWindow cubeShape projMat viewMat

renderSimulation :: (MonadIO m, MonadState World m) 
                 => Window -> Shape Uniforms -> M44 GLfloat -> M44 GLfloat -> m ()
renderSimulation window cubeShape projMat viewMat = do
    let Uniforms{..} = sUniforms cubeShape
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    
    uniformV3 uCamera =<< use (wldPlayer . posPosition)

    let viewProj = projMat !*! viewMat

    withVAO (sVAO cubeShape) $ do
        cubes <- Map.toList <$> use wldCubes
        forM_ cubes $ \(cubeID, cube) -> do
            (position, orientation) <- getBodyState (cube ^. cubBody)
            
            mCubeHit <- use (wldCubeHits . at cubeID)
            forM_ mCubeHit $ \cubeHit ->
                uniformV3 uCubeHit cubeHit

            let model = mkTransformation orientation position !*! scaleMatrix (cube ^. cubSize)
            uniformM44 uModelViewProjection (viewProj !*! model)
            uniformM44 uInverseModel        (inv44 model)
            uniformM44 uModel               model
            uniformV4  uDiffuse             (cube ^. cubColor)

            glDrawElements GL_TRIANGLES (geoVertCount (sGeometry cubeShape)) GL_UNSIGNED_INT nullPtr
            
    swapBuffers window

raycastCursorHits :: (MonadIO m, MonadState World m) 
                  => Window -> DynamicsWorld -> M44 GLfloat -> m ()
raycastCursorHits window dynamicsWorld projMat = do
    playerPose <- use wldPlayer
    cursorRay  <- cursorPosToWorldRay window projMat playerPose

    mRayResult <- rayTestClosest dynamicsWorld cursorRay
    forM_ mRayResult $ \rayResult -> do
        bodyID <- getRigidBodyID (rrRigidBody rayResult)
        mCube <- use (wldCubes . at (fromIntegral (unRigidBodyID bodyID)))
        forM_ mCube $ \_cube -> do          
            
            -- Convert the hit location into model space
            -- (position, orientation) <- getBodyState (cube ^. cubBody)
            -- let model = mkTransformation orientation position
            --     pointOnModel = worldPointToModelPoint model (rrLocation rayResult)
            let worldHit = rrLocation rayResult
                      
            let cubeID = fromIntegral (unRigidBodyID bodyID)
            [r,g,b] <- liftIO (replicateM 3 randomIO)
            wldCubes . at cubeID . traverse . cubColor .= V4 r g b 1

            wldCubeHits . at cubeID ?= worldHit
