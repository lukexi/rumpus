# RUMPUS MANUAL

Ok, so you're ready to rump. Rumping is not for the faint-hearted; nothing is finished, everything is subject to change. I don't think typing letters into virtual screens in VR is a "good idea"*. But it's still a heck of a good time and you'll probably have some fun.

> *Higher-level "VR native" languages are underway, but I wanted a development environment and "assembly language" first, and Rumpus is a snapshot of that process.

### RUMPUS BASICS
When you boot up Rumpus you'll be in the **Scene Loader**. Choose one of the orbs around you to visit that world.

Pick up objects by grabbing them with the trigger. Hold the grip buttons to bring up the **Teleport Beam** - point it at platforms and release when it turns green and starts to rumble to teleport to that location.

Press and hold the "App Button" (above the touchpad on your Vive controller) on one hand to bring up the **Object Bouquet**. Pull objects out of the bouquet with your other hand to bring them into the world. To destroy an object, drag it into the center of the Bouquet and release.

To return to the **Scene Loader**, click the green orb that appears at the bottom of the wand when holding the App Button with your other hand.

You can create your own scene by clicking the "New Scene" orb in the **Scene Loader**. Scenes are persistent, so you can build arrangements and revist them later.

To learn to edit and create your own objects, see _RUMPUS PROGRAMMING GUIDE_ below.

### RUMPUS MULTIPLAYER
This is currently a hack with many caveats. You shouldn't do this with anyone you don't know, as you're going to be running arbitrary code that can do absolutely anything. Better things are in the pipeline. But again, it's extremely fun anyway and you should totally try it if you have friends you can trust.

How it's done:
The first time you run Rumpus, it creates a folder in `My Documents/Rumpus`. Inside that folder, you'll find a file called redirect.txt, and inside that file, you'll see an example path like
`c:/Users/MY_USERNAME/Dropbox/MY_SHARED_FOLDER`.

Create a shared Dropbox folder with your friend (Rumpus doesn't know anything about Dropbox, so you can also use any service that has similar capabilities), and type the path to that folder into `redirect.txt`. Have your friend do the same.

Launch Rumpus, and everything you do will now be synchronized to your friend's computer. Creating, moving, and deleting objects, creating scenes, and editing code will all be synced - usually within 100ms or less. This should theoretically work with any number of players (provided you try not to step on each others toes by e.g. editing the same file simultaneously), and since it's just a Dropbox folder, it doesn't matter if everyone is "online" when you're working; changes will be synced regardless and your friends will see them next time they boot up Rumpus.
> Note: transient state like the physics simulation is not synced - that'll be coming soon in the "real" Rumpus multiplayer implementation.

You don't have to do any coding to do the multiplayer; you can, for example, use the Note and Playhead objects to collaboratively compose music.

Or, if you know a code wizard, you can just hang out with them and play with what they make.

Rumpus has no built-in voice chat yet, so I use Steam's voice chat.

To return to single-player mode or change who you're working with, just edit or delete `redirect.txt`.

## RUMPUS PROGRAMMING GUIDE

Rumpus is a game engine, a code editor and a compiler (with bindings to _Bullet Physics_ for physics, and _Pure Data_ for DSP). In this initial alpha, objects are programmed using pure Haskell along with the Rumpus API.

Again with the caveats: the Rumpus programming API is highly in flux, and is in no way a good piece of design in the snapshot you're using. And again with the counter-caveats: you can do a lot with it regardless - just be ready to rewrite some or all of it as Rumpus evolves : ).

### Creating your first object
Hold the "App Button" on your Vive controller (just above the touchpad) to bring out the **Object Bouquet**. Look for the light-colored slab that says "New Object", grab it, and drag it out. You're done!

Click on the object you just created to begin editing the code. You'll see some boilerplate:
```
module MyObject1 where
import Rumpus

start :: Start
start = do
    return ()

```
> Before we go on, note that you have a few options for editing this code. Using the touchpads is convenient for spot edits while immersed in the world, but is quite tedious for extended typing. Rumpus works wonderfully with a wireless keyboard; just pull the slab to a convenient location in your room, find your keyboard using the Vive's "Tron Mode", and clack away. This is how I built most of the objects in Rumpus. You can also jump out of VR and edit Rumpus code in your favorite text editor - you'll still get all the convenience of Rumpus's live-reloading system every time you save the file. Or, try a hybrid using SteamVR's Desktop mode! You'll find the file you're editing in `My Documents/Rumpus/0.#.#/MyObjectX.hs`.

> Change the name at the top of the file (after module) to rename the object and its associated .hs file.

The `start` function is called once whenever a new instance of your object is created, and again each time you make a valid change to the code.

Each time you type a character, Rumpus rapidly recompiles and re-runs the start function, allowing for highly interactive development.

Try changing the start function to:
```
start = do
    myColor ==> colorHSL 0.5 0.7 0.7
```
You'll see a panel appear as you type showing the current errors in the file (which are inevitable when you're still typing the code). When it disappears, it means the object compiled successfully.
Flip your object over and you should see that it's changed to a lovely shade of blue.

> Rumpus is structured as an "Entity Component System".
Entities are all of the distinct physical objects in the world, like the one you just created.
Components are the properties that define how an entity looks and behaves, like myColor in the example above, or myShape, or mySize, etc.
Systems take entities and their components and make them go, performing tasks like Rendering, Physics, and Sound. In the current version, Systems are "behind the scenes", but you'll eventually be able to modify and build them too.


You can create entities in two ways: by grabbing them out of the Object Bouquet, or by creating them programatically.
To create a new entity via code, change the start function to:
```
start = do
    spawnChild $ do
        myColor      ==> colorHSL 0.5 0.7 0.7
        myShape      ==> Sphere
        myPose       ==> position (V3 0 1 0)
        mySize       ==> 0.1
```

A sphere should appear above your code slab. Try editing the size, pose, shape, and color to see how it updates as you type.

> Calling `spawnChild` establishes a 'parent-child' relationship between two entities;
in this case, between the code slab you're editing and the sphere you just brought into being.
Each time the code is edited, just before the `start` function is re-run, all child objects are removed,
such that new ones created by the your changed `start` function can take their place - usually so quickly
that you won't notice.
By default, child objects will be positioned relative to their parents.

You can give life to our entity in a variety of ways. The swiss-army knife of giving life is the `myUpdate` component.
Change your start function to:
```
start = do
    spawnChild $ do
        myColor      ==> colorHSL 0.5 0.7 0.7
        myShape      ==> Sphere
        myPose       ==> position (V3 0 1 0)
        mySize       ==> 0.1
        myUpdate     ==> do
            t <- getNow
            setPosition (V3 0 (sin t) 0)
            setColor (colorHSL (sin (t/2)) 0.7 0.7)
```

You can call spawnChild as many times as you like. Let's create a field of dreams:
```
start = do
    forM_ [0..100] $ \i -> do
        let x = i `mod` 10
            z = i `div` 10
        spawnChild $ do
            myColor      ==> colorHSL 0.5 0.7 0.7
            myShape      ==> Sphere
            myPose       ==> position (V3 0 1 0)
            mySize       ==> 0.1
            myUpdate     ==> do
                t <- getNow
                let t2 = t + i
                setPosition (V3 x (sin t2) z)
                setColor (colorHSL (sin (t2/2)) 0.7 0.7)
```

### ADDING KNOBS
You can parameterize your object by adding Knobs.
To create a simple knob, call `createKnob` with a name and a range of values.
Knobs will appear along the sides of the code slab.
You can read the value of the knob in your myUpdate function using readKnob.
Change your start function to add speed and height knobs:
```
start = do
    speedKnob  <- createKnob "Speed"  (0.1, 10)
    heightKnob <- createKnob "Height" (0.1, 10)

    forM_ [0..100] $ \i -> do
        let x = i `mod` 10
            z = i `div` 10
        spawnChild $ do
            myColor      ==> colorHSL 0.5 0.7 0.7
            myShape      ==> Sphere
            myPose       ==> position (V3 0 1 0)
            mySize       ==> 0.1
            myUpdate     ==> do
                speed  <- readKnob speedKnob
                height <- readKnob heightKnob
                t <- getNow
                let t2 = t * speed + i
                setPosition (V3 x (sin t2 * height) z)
                setColor (colorHSL (sin (t2/2)) 0.7 0.7)
```

Knobs are cool because their values are saved as persistent state, which means they are also sent across in multiplayer.

### INTERACTION AND PHYSICS
To create your own interactive objects, your object must have a myBody component.

Adding a body adds an entity to the physics simulation so it can react to gravity, collisions, or objects passing through it (like your hand).
Rumpus integrates the Bullet Physics Engine for rigid body dynamics.

To spawn a basic physics object, try a start function like this:
```
start = do
    spawnChild $ do
        myColor      ==> colorHSL 0.5 0.7 0.7
        myShape      ==> Cube
        myPose       ==> position (V3 0 1 0)
        mySize       ==> 0.1
        myBody       ==> Physical
```
The object will fall into the void. Try pulling out a Platform object for it to land on.

There are 3 kinds of physics body you can request in `myBody`:
* `Physical` creates a "real-world" object that can be picked up, thrown, knocked around, etc.
    You can change these objects' Mass, Restitution, and Gravity using myMass, myRestitution, and myGravity
    (by default, they'll experience an Earth gravity of 9.8m/s^2)
* `Animated` creates a physics object that you animate yourself (e.g. with `myUpdate`).
    It is still a solid object, so it will knock other Physical objects around, but has effectively infinite mass and thus won't be affected by them.
    This corresponds to "Kinematic" in Bullet Physics.
* `Detector` creates an ethereal physics object that detects when objects are intersecting it but passes through them like a ghost.
    This corresponds to "NoContactResponse" in Bullet Physics.

To react when the object hits another object (including your hands), use the myCollisionBegan, myCollisionContinues, and myCollisionEnded components.
For example, to change color every time we touch something:
```
start = do
    spawnChild $ do
        myColor          ==> colorHSL 0.5 0.7 0.7
        myShape          ==> Cube
        myPose           ==> position (V3 0 1 0)
        mySize           ==> 0.1
        myBody           ==> Physical
        myCollisionBegan ==> \_ _ -> do
            hue <- randomRange (0,1)
            setColor (colorHSL hue 0.5 0.5)
```
Adding a physics body also makes myPose refer to absolute rather than relative coordinates.

### ATTACHMENT
You can attach physics bodies to one another with attachEntity like so:
```
start = do
    childID <- spawnChild $ do
        myColor          ==> colorHSL 0.5 0.7 0.7
        myShape          ==> Cube
        myPose           ==> position (V3 0 1 0)
        mySize           ==> 0.1
        myBody           ==> Animated
        myCollisionBegan ==> \_ _ -> do
            hue <- randomRange (0,1)
            setColor (colorHSL hue 0.5 0.5)
    attachEntity childID (position (V3 0 0.5 0))
```
The second argument to `attachEntity` is the pose offset at which the entity should be attached.

### DRAGGING
Entities with myBody set will receive myDragBegan, myDragContinues, and myDragEnded events when dragged by the hand controllers. You can use this to implement interactive objects. If you'd like to handle dragging yourself rather than using the built-in behavior of attaching entities to the hand controllers when they are dragged, you can set `myDragOverride ==> True` on your entity.



### TELEPORTATION
You can mark any physics object as "Teleportable" to allow yourself to teleport on top of it using the Grip (side) buttons on the Vive controller.
```
start = do
    spawnChild $ do
        myColor      ==> colorHSL 0.5 0.7 0.7
        myShape      ==> Cube
        myPose       ==> position (V3 0 0 -1)
        mySize       ==> 0.1
        myBodyFlags  ==> [Teleportable]
```

### SYNTHESIS
Every entity in Rumpus can have a synthesizer attached to it. The sound will radiate from that entity's position in the world.

Synthesizers are defined using the Pure Data DSP language. To start with, you can use one of the included synthesizers.

Try this:
```
majorScale = map (+56) [0,2,4,7,9]

start = do
    setSynthPatch "Fountain.pd"

    myCollisionBegan ==> \_ _ -> do
        note <- randomFrom majorScale
        sendSynth "note" (realToFrac note)
```
The code slab will now play a random note from the major scale each time it is touched.

This works well for long-running synthesis patches, but is inefficient for objects with many transient instances or sounds
(for example, we want to spawn possibly hundreds of Note objects, but we don't want to spawn hundreds of "Note" synthesizers)

Instead, we can use `acquirePolyPatch` like so:
```
majorScale = map (+56) [0,2,4,7,9]

start = do
    myCollisionBegan ==> \_ _ -> do
        note <- randomFrom majorScale
        acquirePolyPatch "Note.pd"
        sendSynth "note" (fromIntegral note)
        sendSynth "trigger" 1
```
This will create a bank of 8 Note.pd instances, and assign them on demand to the most recent entity to call acquirePolyPatch.



### THE DIFFERENCE BETWEEN `myFoo ==> Bar` AND `setFoo Bar`

The basic rule of thumb is that `myFoo ==> Bar` is used when defining an object (i.e., within a spawnChild block),
and `setFoo Bar` is used to modify an already existing object (e.g. in a `myUpdate` function).
For cases where no side effects are needed (or wanted), ==> can still be used.

This is a historical accident and will be fixed in a future version.
(The reason they both exist: ==> is "side-effect free" and simply sets a component value directly, which is desirable when an object is being constructed, whereas the `set` functions trigger necessary side effects on existing objects, such as recreating physics bodies when their shape or size is changed)









































