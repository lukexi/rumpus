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
import Sound.Pd
import Data.Map (Map)
import GHC.Word
import Control.Monad.State
import Control.Monad.Reader

type EntityID = Word32

type EntityMap a = Map EntityID a

data ShapeType = NoShape | CubeShape | SphereShape | StaticPlaneShape deriving (Eq, Show, Ord, Enum)

data PhysicsProperties = IsKinematic | IsGhost deriving (Eq, Show)

type WorldMonad = StateT World (ReaderT WorldStatic IO)



data WorldStatic = WorldStatic
    { _wlsVRPal         :: !VRPal
    , _wlsDynamicsWorld :: !DynamicsWorld
    , _wlsPd            :: !PureData
    , _wlsShapes        :: ![(ShapeType, Shape Uniforms)]
    }

data World = World
    { _wldPlayer     :: !(Pose GLfloat)
    , _wldComponents :: !Components
    , _wldEvents     :: ![WorldEvent]
    }

data ButtonState = ButtonDown | ButtonUp

data HandButton = HandButtonA
                | HandButtonB
                | HandButtonC
                | HandButtonD
                | HandButtonStart
                | HandButtonGrip
                | HandButtonTrigger

data WhichHand = LeftHand | RightHand

data HandEvent = HandStateEvent  Hand
               | HandButtonEvent HandButton ButtonState

data VREvent = HeadEvent (M44 GLfloat)
             | HandEvent WhichHand HandEvent

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
    , _cmpParents     :: EntityMap EntityID
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
    , _cmpParents     = mempty
    }

newWorld :: World
newWorld = World
    { _wldPlayer = Pose (V3 0 1 5) (axisAngle (V3 0 1 0) 0)
    , _wldComponents = newComponents
    , _wldEvents = []
    }

newEntity :: Entity
newEntity = Entity
    { _entColor     = V4 1 1 1 1
    , _entSize      = V3 1 1 1
    , _entPose      = newPose
    , _entScale     = V3 1 1 1
    , _entRigidBody = Nothing
    , _entUpdate    = Nothing
    , _entPhysProps = []
    , _entShape     = NoShape
    , _entChildren  = []
    , _entMass      = 1
    , _entPdPatch   = Nothing
    }

data Entity = Entity
    { _entColor     :: !(V4 GLfloat)
    , _entSize      :: !(V3 GLfloat)
    , _entPose      :: !(Pose GLfloat)
    , _entScale     :: !(V3 GLfloat)
    , _entRigidBody :: !(Maybe RigidBody)
    , _entUpdate    :: !(Maybe (EntityID -> WorldMonad ()))
    , _entPhysProps :: ![PhysicsProperties]
    , _entShape     :: !(ShapeType)
    , _entChildren  :: ![Entity]
    , _entMass      :: !Float
    , _entPdPatch   :: !(Maybe FilePath)
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


raycastCursorHits :: (MonadIO m, MonadState World m) 
                  => Window -> DynamicsWorld -> M44 GLfloat -> m ()
raycastCursorHits window dynamicsWorld projMat = do
    playerPose <- use wldPlayer
    cursorRay  <- cursorPosToWorldRay window projMat playerPose

    mRayResult <- rayTestClosest dynamicsWorld cursorRay
    forM_ mRayResult $ \rayResult -> do
        bodyID <- getCollisionObjectID (rrCollisionObject rayResult)
        -- Convert the hit location into model space
        -- (position, orientation) <- getBodyState (cube ^. cubBody)
        -- let model = mkTransformation orientation position
        --     pointOnModel = worldPointToModelPoint model (rrLocation rayResult)
        let _hitInWorld = rrLocation rayResult
            entityID = fromIntegral (unCollisionObjectID bodyID) :: EntityID
        return entityID
