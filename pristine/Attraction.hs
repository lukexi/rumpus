module Attraction where
import Rumpus

main = do

    bodies <- forM spawnChild $ do
        myGravity ==> 0
        myBody ==> Physical
        myShape ==> Cube
        mySize ==> 0.1

    let target = V3 0 0 0
    myUpdate ==> do
        forM_ bodyIDs $ \body -> do
            position <- getEntityPosition body
            let orientation = normalize (target - (position :: V3 GLfloat)) :: V3 GLfloat
            applyForceToEntity body orientation
            setColor (1 & _xyz .~ orientation)
