{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
module Types where
import Control.Lens.Extra
import Linear.Extra
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.VR.Pal
import Physics.Bullet
import Data.Map (Map)
import GHC.Word
import Control.Monad.State
import Control.Monad.Reader

type EntityID = Word32

type EntityMap a = Map EntityID a

data ShapeType = None | Cube | Sphere

data PhysicsProperties = IsKinematic | IsGhost deriving (Eq, Show)

type WorldMonad = StateT World (ReaderT WorldStatic IO)



data WorldStatic = WorldStatic
    { _wlsDynamicsWorld :: !DynamicsWorld
    , _wlsCubeShape     :: !(Shape Uniforms)
    , _wlsVRPal         :: !VRPal
    }

data World = World
    { _wldPlayer     :: !(Pose GLfloat)
    , _wldComponents :: !Components
    , _wldEvents     :: ![WorldEvent]
    }

data ButtonDown = ButtonDown | ButtonUp

data HandEvent = HandEvent Hand
               | ButtonA       ButtonDown
               | ButtonB       ButtonDown
               | ButtonC       ButtonDown
               | ButtonD       ButtonDown
               | ButtonGrip    ButtonDown
               | ButtonStart   ButtonDown
               | ButtonTrigger ButtonDown

data VREvent = HeadEvent (M44 GLfloat)
             | LeftHandEvent HandEvent
             | RightHandEvent HandEvent

data WorldEvent = GLFWEvent Event
                | VREvent VREvent

data Components = Components
    { _cmpPose        :: EntityMap (Pose GLfloat)
    , _cmpSize        :: EntityMap (V3 GLfloat)
    , _cmpScale       :: EntityMap (V3 GLfloat)
    , _cmpColor       :: EntityMap (V4 GLfloat)
    , _cmpShape       :: EntityMap ShapeType
    , _cmpRigidBody   :: EntityMap RigidBody
    , _cmpGhostObject :: EntityMap GhostObject
    , _cmpUpdate      :: EntityMap (EntityID -> WorldMonad ())
    }

newComponents :: Components
newComponents = Components
    { _cmpPose        = mempty
    , _cmpSize        = mempty
    , _cmpScale       = mempty
    , _cmpColor       = mempty
    , _cmpRigidBody   = mempty
    , _cmpUpdate      = mempty
    , _cmpGhostObject = mempty
    , _cmpShape       = mempty
    }

data Entity = Entity
    { _entColor     :: !(V4 GLfloat)
    , _entSize      :: !(V3 GLfloat)
    , _entPose      :: !(Pose GLfloat)
    , _entScale     :: !(V3 GLfloat)
    , _entRigidBody :: !(Maybe RigidBody)
    , _entUpdate    :: !(Maybe (EntityID -> WorldMonad ()))
    , _entPhysProps :: [PhysicsProperties]
    , _entShape     :: ShapeType
    }




data Uniforms = Uniforms
    { uModelViewProjection :: UniformLocation (M44 GLfloat)
    , uInverseModel        :: UniformLocation (M44 GLfloat)
    , uModel               :: UniformLocation (M44 GLfloat)
    , uCamera              :: UniformLocation (V3  GLfloat)
    , uDiffuse             :: UniformLocation (V4  GLfloat)
    , uCubeHit             :: UniformLocation (V3  GLfloat)
    } deriving (Data)


makeLenses ''WorldStatic
makeLenses ''World
makeLenses ''Entity
makeLenses ''Components



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
