{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
module Rumpus.Types where
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
import Control.Concurrent
import Data.Yaml
import Data.Aeson.Types
import GHC.Generics
import Data.Foldable
import TinyRick

traverseM :: (Monad m, Traversable t) => m (t a) -> (a -> m b) -> m (t b)
traverseM f x = f >>= traverse x
traverseM_ :: (Monad m, Foldable t) => m (t a) -> (a -> m b) -> m ()
traverseM_ f x = f >>= traverse_ x

instance FromJSON a => FromJSON (V4 a)
instance FromJSON a => FromJSON (V3 a)
instance FromJSON a => FromJSON (V2 a)
instance FromJSON a => FromJSON (Quaternion a)
instance ToJSON a => ToJSON (V4 a)
instance ToJSON a => ToJSON (V3 a)
instance ToJSON a => ToJSON (V2 a)
instance ToJSON a => ToJSON (Quaternion a)

poseJSONOptions :: Options
poseJSONOptions = defaultOptions { fieldLabelModifier = drop 4 }

instance FromJSON a => FromJSON (Pose a) where
    parseJSON = genericParseJSON poseJSONOptions
instance ToJSON a => ToJSON (Pose a) where
    toJSON     = genericToJSON poseJSONOptions

type EntityID = Word32

type EntityMap a = Map EntityID a

data ShapeType = NoShape | CubeShape | SphereShape | StaticPlaneShape deriving (Eq, Show, Ord, Enum, Generic, FromJSON, ToJSON)

data PhysicsProperties = IsKinematic | IsGhost deriving (Eq, Show, Generic, FromJSON, ToJSON)

type WorldMonad = StateT World (ReaderT WorldStatic IO)

data WorldEvent = GLFWEvent Event
                | VREvent VREvent 
                deriving Show

data Persistence = Transient | Persistent deriving (Eq, Show, Generic, FromJSON, ToJSON)

type OnUpdate = EntityID -> WorldMonad ()

type CollidedWithID = EntityID
type CollisionImpulse = GLfloat
type OnCollision = EntityID -> CollidedWithID -> CollisionImpulse -> WorldMonad ()

data WorldStatic = WorldStatic
    { _wlsVRPal         :: !VRPal
    , _wlsDynamicsWorld :: !DynamicsWorld
    , _wlsPd            :: !PureData
    , _wlsShapes        :: ![(ShapeType, Shape Uniforms)]
    , _wlsFont          :: !Font
    , _wlsGHCChan       :: !(Chan (CompilationRequest (EntityID -> WorldMonad ())))
    }

data World = World
    { _wldPlayer           :: !(Pose GLfloat)
    , _wldPlayerHeadM44    :: !(M44 GLfloat)
    , _wldComponents       :: !Components
    , _wldEvents           :: ![WorldEvent]
    , _wldOpenALSourcePool :: ![(Int, OpenALSource)]
    , _wldPlaying          :: !Bool
    , _wldEntityLibrary    :: !(Map String Entity)
    , _wldScene            :: !(Map EntityID Entity)
    }

-- not yet used
data Scene = Scene
    { _scnName :: String
    , _scnEntities :: !(Map EntityID Entity)
    }

newWorld :: World
newWorld = World
    { _wldPlayer = Pose (V3 0 0 0) (axisAngle (V3 0 1 0) 0)
    , _wldPlayerHeadM44 = identity
    , _wldComponents = newComponents
    , _wldEvents = []
    , _wldOpenALSourcePool = []
    , _wldPlaying = False
    , _wldEntityLibrary = mempty
    , _wldScene = mempty
    }

data Attachment = Attachment EntityID (Pose GLfloat)

data Components = Components
    { _cmpName        :: EntityMap String
    , _cmpPose        :: EntityMap (Pose GLfloat)
    , _cmpSize        :: EntityMap (V3 GLfloat)
    , _cmpShape       :: EntityMap ShapeType
    , _cmpScale       :: EntityMap (V3 GLfloat)
    , _cmpColor       :: EntityMap (V4 GLfloat)
    , _cmpScript      :: EntityMap (Editor OnUpdate)
    , _cmpParent      :: EntityMap EntityID
    , _cmpRigidBody   :: EntityMap RigidBody
    , _cmpGhostObject :: EntityMap GhostObject
    , _cmpSpring      :: EntityMap SpringConstraint
    , _cmpPdPatch     :: EntityMap Patch
    , _cmpSoundSource :: EntityMap OpenALSource
    , _cmpAttachment  :: EntityMap Attachment
    }

-- not yet used
data PhysicsComponents = PhysicsComponents
    { _pcRigidBody   :: EntityMap RigidBody
    , _pcGhostObject :: EntityMap GhostObject
    , _pcSpring      :: EntityMap SpringConstraint
    , _pcCollision   :: EntityMap OnCollision
    }

newComponents :: Components
newComponents = Components
    { _cmpName        = mempty
    , _cmpPose        = mempty
    , _cmpSize        = mempty
    , _cmpShape       = mempty
    , _cmpScale       = mempty
    , _cmpColor       = mempty
    , _cmpScript      = mempty
    , _cmpParent      = mempty
    , _cmpRigidBody   = mempty
    , _cmpGhostObject = mempty
    , _cmpSpring      = mempty
    , _cmpPdPatch     = mempty
    , _cmpSoundSource = mempty
    , _cmpAttachment  = mempty
    }

data Entity = Entity
    { _entColor     :: !(V4 GLfloat)
    , _entSize      :: !(V3 GLfloat)
    , _entPose      :: !(Pose GLfloat)
    , _entScale     :: !(V3 GLfloat)
    , _entPhysProps :: ![PhysicsProperties]
    , _entShape     :: !(ShapeType)
    , _entChildren  :: ![Entity]
    , _entMass      :: !Float
    , _entPdPatch   :: !(Maybe FilePath)
    , _entScript    :: !(Maybe FilePath)
    , _entName      :: !String
    } deriving (Show, Generic)

entityJSONOptions :: Options
entityJSONOptions = defaultOptions { fieldLabelModifier = drop 4 }

instance FromJSON Entity where
    parseJSON = genericParseJSON entityJSONOptions
instance ToJSON Entity where
    toJSON     = genericToJSON entityJSONOptions

newEntity :: Entity
newEntity = Entity
    { _entColor     = V4 1 1 1 1
    , _entSize      = V3 1 1 1
    , _entPose      = newPose
    , _entScale     = V3 1 1 1
    , _entPhysProps = []
    , _entShape     = NoShape
    , _entChildren  = []
    , _entMass      = 1
    , _entPdPatch   = Nothing
    , _entScript    = Nothing
    , _entName      = "Entity"
    }


data Uniforms = Uniforms
    { uModelViewProjection :: UniformLocation (M44 GLfloat)
    , uInverseModel        :: UniformLocation (M44 GLfloat)
    , uModel               :: UniformLocation (M44 GLfloat)
    , uCamera              :: UniformLocation (V3  GLfloat)
    , uDiffuse             :: UniformLocation (V4  GLfloat)
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
