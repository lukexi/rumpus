module DefaultCollisionStart where
import Rumpus

collisionStart :: CollisionStart
collisionStart entityID withEntityID impulse = do
    hue <- liftIO randomIO
    setEntityColor (hslColor hue 0.8 0.4) entityID
