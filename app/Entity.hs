{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Entity where
import Control.Lens.Extra
import Linear.Extra
import Graphics.GL.Pal
import Graphics.VR.Pal
import Physics.Bullet
import Data.Map (Map)
import GHC.Word
import Control.Monad.State
import System.Random
import Control.Monad.Reader

data Uniforms = Uniforms
    { uModelViewProjection :: UniformLocation (M44 GLfloat)
    , uInverseModel        :: UniformLocation (M44 GLfloat)
    , uModel               :: UniformLocation (M44 GLfloat)
    , uCamera              :: UniformLocation (V3  GLfloat)
    , uDiffuse             :: UniformLocation (V4  GLfloat)
    , uCubeHit             :: UniformLocation (V3  GLfloat)
    } deriving (Data)

type EntityID = Word32
type EntityMap a = Map EntityID a

data Entity = Entity
  { _entColor     :: !(V4 GLfloat)
  , _entSize      :: !(V3 GLfloat)
  , _entPose      :: !(Pose GLfloat)
  , _entScale     :: !(V3 GLfloat)
  , _entRigidBody :: !(Maybe RigidBody)
  , _entUpdate    :: !(Maybe (Entity -> IO Entity))
  }
makeLenses ''Entity


data WorldStatic = WorldStatic
    { _wlsDynamicsWorld :: !DynamicsWorld
    , _wlsCubeShape     :: !(Shape Uniforms)
    , _wlsVRPal         :: !VRPal
    }
makeLenses ''WorldStatic

data World = World
    { _wldPlayer    :: !(Pose GLfloat)
    , _wldEntities  :: !(EntityMap Entity)
    }
makeLenses ''World

newWorld :: World
newWorld = World
    { _wldPlayer = Pose (V3 0 20 60) (axisAngle (V3 0 1 0) 0)
    , _wldEntities = mempty
    }


newEntity :: Entity
newEntity = Entity
    { _entColor     = V4 1 1 1 1
    , _entSize      = V3 1 1 1
    , _entPose      = newPose
    , _entScale     = V3 1 1 1
    , _entRigidBody = Nothing
    , _entUpdate    = Nothing
    }



setEntitySize :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) => EntityID -> V3 GLfloat -> m ()
setEntitySize entityID newSize = do
    use (wldEntities . at entityID) >>= mapM_ (\entity -> do
        forM_ (entity ^. entRigidBody) $ \rigidBody -> do
            dynamicsWorld <- view wlsDynamicsWorld
            setCubeScale dynamicsWorld rigidBody newSize
        return (entity & entSize .~ newSize)
        )


data IsKinematic = IsKinematic | IsNotKinematic deriving (Eq, Show)



createEntity :: (MonadIO m, MonadState World m) => Entity -> m EntityID
createEntity entity = do
    entityID <- liftIO randomIO
    wldEntities . at entityID ?= entity
    return entityID

addEntityRigidBodyComponent :: (MonadIO m, MonadState World m, MonadReader WorldStatic m) 
                            => EntityID -> IsKinematic -> m ()
addEntityRigidBodyComponent entityID isKinematic = do

    dynamicsWorld <- view wlsDynamicsWorld

    use (wldEntities . at entityID) >>= mapM_ (\entity -> do
        rigidBody <- addCube dynamicsWorld (RigidBodyID entityID) 
            mempty { pcPosition = entity ^. entPose . posPosition
                   , pcRotation = entity ^. entPose . posOrientation
                   , pcScale    = entity ^. entSize
                   }
        when (isKinematic == IsKinematic) 
            (setRigidBodyKinematic rigidBody)
        wldEntities . at entityID . traverse . entRigidBody ?= rigidBody
        )


