import Physics.Bullet


data System a = System 
    { sysInit :: IO a
    , sysUpdate :: a -> IO a
    , sysDestroy :: a -> IO ()
    }


createPhysicsSystem :: System DynamicsWorld
createPhysicsSystem = System 
    { sysInit = createDynamicsWorld mempty
    , sysUpdate = \dynamicsWorld -> stepSimulation dynamicsWorld 90 >> return dynamicsWorld
    , sysDestroy = \_dynamicsWorld -> return ()
    }
