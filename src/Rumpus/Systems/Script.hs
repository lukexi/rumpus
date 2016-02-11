{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Rumpus.Systems.Script where
import PreludeExtra
import Rumpus.Types
import Data.ECS

-- | OnStart function
type OnStart = EntityID -> ECSMonad (Maybe Dynamic)

nullOnStart :: OnStart
nullOnStart _entityID = return Nothing


-- | OnUpdate function
type OnUpdate = EntityID -> ECSMonad ()

nullOnUpdate :: OnUpdate
nullOnUpdate _entityID = return ()



-- | OnCollision functions
type CollidedWithID = EntityID
type CollisionImpulse = GLfloat
type OnCollision = EntityID -> CollidedWithID -> CollisionImpulse -> ECSMonad ()

nullOnCollision :: OnCollision
nullOnCollision _entityID _collidedWithID _collisionImpulse = return ()


defineComponentKey ''OnStart
defineComponentKey ''OnUpdate
defineComponentKey ''OnCollision
defineComponentKeyWithType "ScriptData" [t|Dynamic|]

tickScriptingSystem :: ECSMonad ()
tickScriptingSystem = do
    forEntitiesWithComponent cmpOnStart $
        \(entityID, onStart) -> do
            -- Only call OnStart once
            mScriptData <- onStart entityID
            forM_ mScriptData $ \scriptData -> 
                addComponent cmpScriptData scriptData entityID
            removeComponent cmpOnStart entityID

    forEntitiesWithComponent cmpOnUpdate $
        \(entityID, onUpdate) -> 
            onUpdate entityID



withScriptData :: (Typeable a, MonadIO m, MonadState ECS m) =>
                    EntityID -> (a -> m ()) -> m ()
withScriptData entityID f = 
    withComponent entityID cmpScriptData $ \dynScriptData -> do
        case fromDynamic dynScriptData of
            Just scriptData -> f scriptData
            Nothing -> putStrLnIO 
                ("withScriptData: Attempted to use entityID " ++ show entityID 
                    ++ "'s script data of type " ++ show dynScriptData 
                    ++ " with a function that accepts a different type.")

editScriptData :: (Typeable a, MonadIO m, MonadState ECS m) =>
                    EntityID -> (a -> m a) -> m ()
editScriptData entityID f = 
    modifyComponent entityID cmpScriptData $ \dynScriptData -> do
        case fromDynamic dynScriptData of
            Just scriptData -> toDyn <$> f scriptData
            Nothing -> do
                putStrLnIO 
                    ("editScriptData: Attempted to use entityID " ++ show entityID 
                        ++ "'s script data of type " ++ show dynScriptData 
                        ++ " with a function that accepts a different type.")
                return dynScriptData
