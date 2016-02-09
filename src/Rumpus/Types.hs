{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
module Rumpus.Types where
import PreludeExtra

import GHC.Word
import Rumpus.Orphans ()


import Data.Vault.Strict (Vault)

type ComponentName = String

-- data ComponentInterface = ComponentInterface
--     { ciAddComponent     :: (EntityID -> WorldMonad ())
--     , ciRemoveComponent  :: (EntityID -> WorldMonad ())
--     , ciExtractComponent :: Maybe (EntityID -> WorldMonad (Maybe Value))
--     }
data ComponentInterface = ComponentInterface
    { ciAddComponent     :: forall m. (MonadState World m, MonadIO m) => (EntityID -> m ())
    , ciRemoveComponent  :: forall m. (MonadState World m, MonadIO m) => (EntityID -> m ())
    , ciExtractComponent :: forall m. (MonadState World m, MonadIO m) => Maybe (EntityID -> m (Maybe Value))
    }

type EntityID = Word32

type EntityMap a = Map EntityID a




type WorldMonad = StateT World IO




data World = World
    { _wldSystems            :: Vault
    , _wldComponents         :: Vault
    , _wldComponentLibrary   :: Map ComponentName ComponentInterface
    , _wldEntities           :: [EntityID]
    }



newWorld :: World
newWorld = World
    { _wldComponents         = mempty
    , _wldSystems            = mempty
    , _wldComponentLibrary   = mempty
    , _wldEntities           = mempty    
    }

makeLenses ''World

type HandEntityID = EntityID
{-
data Entity = Entity
    { _entName              :: !String
    , _entSize              :: !(V3 GLfloat)
    , _entShape             :: !(ShapeType)
    , _entPose              :: !(Pose GLfloat)
    , _entColor             :: !(V4 GLfloat)
    , _entPhysicsProperties :: ![PhysicsProperties]
    , _entChildren          :: ![Entity]
    , _entMass              :: !Float
    , _entLifetime          :: !(Maybe Float)
    , _entPdPatch           :: !(Maybe FilePath)
    , _entOnStart           :: !(Maybe FilePath)
    , _entOnUpdate          :: !(Maybe FilePath)
    , _entOnCollision       :: !(Maybe FilePath)
    } deriving (Show, Generic)

newEntity :: Entity
newEntity = Entity
    { _entName              = "NewEntity"
    , _entSize              = V3 1 1 1
    , _entShape             = NoShape
    , _entPose              = newPose
    , _entColor             = V4 1 1 1 1
    , _entPhysicsProperties = []
    , _entChildren          = []
    , _entMass              = 1
    , _entLifetime          = Nothing
    , _entPdPatch           = Nothing
    , _entOnStart           = Nothing
    , _entOnUpdate          = Nothing
    , _entOnCollision       = Nothing
    }



entityJSONOptions :: Options
entityJSONOptions = defaultOptions { fieldLabelModifier = drop 4 }

instance FromJSON Entity where
    parseJSON = genericParseJSON entityJSONOptions
instance ToJSON Entity where
    toJSON    = genericToJSON entityJSONOptions
-}




