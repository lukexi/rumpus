module DefaultUpdate where
import Rumpus

update :: OnUpdate
update entityID = do
    now <- getNow 
    setEntityColor (hslColor (sin now) 0.8 0.4) entityID
