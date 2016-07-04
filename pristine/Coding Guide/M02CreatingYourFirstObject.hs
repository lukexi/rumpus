{-
### Creating your first object
Hold the "App Button" on your Vive controller (just above the touchpad)
to bring out the **Object Bouquet**.
Look for the light-colored slab that says "New Object",
grab it, and drag it out. You're done!

Click on the object you just created to begin editing the code.
You'll see some boilerplate like:
```
-}
module M02CreatingYourFirstObject where
import Rumpus

start :: Start
start = do
    return ()

{-
```
> Before we go on, note that you have a few options for editing this code.
Using the touchpads is convenient for spot edits while immersed in the world,
but is quite tedious for extended typing. Rumpus works wonderfully with a
wireless keyboard;just pull the slab to a convenient location in your room,
find your keyboard using the Vive's "Tron Mode", and clack away.
This is how I built most of the objects in Rumpus.
You can also jump out of VR and edit Rumpus code in your favorite
text editor - you'll still get all the convenience of Rumpus's
live-reloading system every time you save the file.
Or, try a hybrid using SteamVR's Desktop mode!

> You'll find the file you're editing in
`My Documents/Rumpus/0.#.#/MyScene/MyObjectX.hs`.

> Change the name at the top of the file (after module) to rename
the object and its associated .hs file.
-}
