{-# LANGUAGE FlexibleContexts #-}
module Mess where
import Rumpus.Entity
import Rumpus.Types
import Linear.Extra
import Graphics.GL.Pal
import Control.Monad.Random

start = do
    stdgen <- liftIO getStdGen
    _ <- flip runRandT stdgen $ do
        forM_ [1..100::Int] $ \_ -> do
            color <- getRandomR (0,1)
            traverseM_ (spawnEntity Transient "MessyBall") $ 
                setEntityColor (color & _w .~ 1)
            traverseM_ (spawnEntity Transient "MessyCube") $
                setEntityColor (color & _w .~ 1)
            traverseM_ (spawnEntity Transient "SoundCube") $ 
                setEntityColor (color & _w .~ 1)
