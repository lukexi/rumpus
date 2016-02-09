{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Script where
import PreludeExtra
import Rumpus.Types
import Rumpus.ECS

-- | OnStart function
type OnStart = EntityID -> WorldMonad (Maybe Dynamic)

nullOnStart :: OnStart
nullOnStart _entityID = return Nothing


-- | OnUpdate function
type OnUpdate = EntityID -> WorldMonad ()

nullOnUpdate :: OnUpdate
nullOnUpdate _entityID = return ()



-- | OnCollision functions
type CollidedWithID = EntityID
type CollisionImpulse = GLfloat
type OnCollision = EntityID -> CollidedWithID -> CollisionImpulse -> WorldMonad ()

nullOnCollision :: OnCollision
nullOnCollision _entityID _collidedWithID _collisionImpulse = return ()


defineComponentKey ''OnStart
defineComponentKey ''OnUpdate
defineComponentKey ''OnCollision
defineComponentKeyWithType "ScriptData" [t|Dynamic|]

scriptingSystem :: WorldMonad ()
scriptingSystem = do
    forEntitiesWithComponent onStartKey $
        \(entityID, onStart) -> do
            -- Only call OnStart once
            mScriptData <- onStart entityID
            forM_ mScriptData $ \scriptData -> 
                addComponent scriptDataKey scriptData entityID
            removeComponentFromEntity onStartKey entityID

    forEntitiesWithComponent onUpdateKey $
        \(entityID, onUpdate) -> 
            onUpdate entityID



withScriptData :: (Typeable a, MonadIO m, MonadState World m) =>
                    EntityID -> (a -> m ()) -> m ()
withScriptData entityID f = 
    withComponent entityID scriptDataKey $ \dynScriptData -> do
        case fromDynamic dynScriptData of
            Just scriptData -> f scriptData
            Nothing -> putStrLnIO 
                ("withScriptData: Attempted to use entityID " ++ show entityID 
                    ++ "'s script data of type " ++ show dynScriptData 
                    ++ " with a function that accepts a different type.")

editScriptData :: (Typeable a, MonadIO m, MonadState World m) =>
                    EntityID -> (a -> m a) -> m ()
editScriptData entityID f = 
    modifyComponent entityID scriptDataKey $ \dynScriptData -> do
        case fromDynamic dynScriptData of
            Just scriptData -> toDyn <$> f scriptData
            Nothing -> do
                putStrLnIO 
                    ("editScriptData: Attempted to use entityID " ++ show entityID 
                        ++ "'s script data of type " ++ show dynScriptData 
                        ++ " with a function that accepts a different type.")
                return dynScriptData
