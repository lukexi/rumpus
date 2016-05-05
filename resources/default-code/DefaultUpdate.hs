module DefaultUpdate where
import Rumpus

update :: Update
update entityID = do
    now <- getNow
    setEntityColor (colorHSL (sin now) 0.8 0.4) entityID
