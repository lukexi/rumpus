module M04CreatingEntities where
import Rumpus
{-
You can create entities in two ways: by grabbing them out of
the Object Bouquet, or by creating them programmatically.
To create a new entity via code, change the start function to:
```
-}
start :: Start
start = do
    spawnChild $ do
        myColor      ==> colorHSL 0.5 0.7 0.7
        myShape      ==> Sphere
        myPose       ==> position (V3 0 1 0)
        mySize       ==> 0.1
{-
```

A sphere should appear above your code slab. Try editing the
size, pose, shape, and color to see how it updates as you type.

> Calling `spawnChild` establishes a 'parent-child' relationship
between two entities; in this case, between the code slab you're editing
and the sphere you just brought into being.
Each time the code is edited, just before the `start` function is re-run,
all child objects are removed, such that new ones created by the your
changed `start` function can take their place - usually so quickly
that you won't notice.
By default, child objects will be positioned relative to their parents.
-}
