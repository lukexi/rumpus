module M03Start where
{-
The `start` function is called once whenever a new instance of your
object is created, and again each time you make a valid change to the code.

Each time you type a character, Rumpus rapidly recompiles and re-runs
the start function, allowing for highly interactive development.

Try changing the start function to:
```
-}
start :: Start
start = do
    myColor ==> colorHSL 0.5 0.7 0.7
{-
```
You'll see a panel appear as you type showing the current errors in
the file (which are inevitable when you're still typing the code).
When it disappears, it means the object compiled successfully.
Flip your object over and you should see that it's
changed to a lovely shade of blue.

> Rumpus is structured as an "Entity Component System".
Entities are all of the distinct physical objects in the world,
like the one you just created.
Components are the properties that define how an entity looks
and behaves, like myColor in the example above, or myShape, or mySize, etc.
Systems take entities and their components and make them go,
performing tasks like Rendering, Physics, and Sound.
In the current version, Systems are "behind the scenes",
but you'll eventually be able to modify and build them too.
-}
