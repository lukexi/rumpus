# RUMPUS PROGRAMMING GUIDE

### Intro

Rumpus is a game engine, a code editor and a compiler
(with bindings to _Bullet Physics_ for physics, and _Pure Data_ for DSP).
In this initial alpha, objects are programmed using pure Haskell
along with the Rumpus API.

Again with the caveats: the Rumpus programming API is highly in flux,
and is in no way a good piece of design in the snapshot you're using.
And again with the counter-caveats: you can do a lot with it
regardless - just be ready to rewrite some or all of it as Rumpus evolves : ).

### Creating your first object
Hold the "App Button" on your Vive controller (just above the touchpad)
to bring out the **Object Bouquet**. Look for the light-colored slab that
says "New Object", grab it, and drag it out. You're done!

Click on the object you just created to begin editing the code.
You'll see some boilerplate like:
```
module MyObject1 where
import Rumpus

start :: Start
start = do
    return ()

```
> Before we go on, note that you have a few options for editing this code.
Using the touchpads is convenient for spot edits while immersed in the world,
but is quite tedious for extended typing. Rumpus works wonderfully with a
wireless keyboard; just pull the slab to a convenient location in your room,
find your keyboard using the Vive's "Tron Mode", and clack away.
This is how I built most of the objects in Rumpus. You can also jump out
of VR and edit Rumpus code in your favorite text editor - you'll still get all
the convenience of Rumpus's live-reloading system every time you save the file.
Or, try a hybrid using SteamVR's Desktop mode!
You'll find the file you're editing in
`My Documents/Rumpus/#.#.#/MyScene/MyObjectX.hs`.

> Change the name at the top of the file (after module) to rename the object
and its associated .hs file.

### Start

The `start` function is called once whenever a new instance of your
object is created, and again each time you make a valid change to the code.

Each time you type a character, Rumpus rapidly recompiles and re-runs
the start function, allowing for highly interactive development.

Try changing the start function to:
```
start :: Start
start = do
    myColor ==> colorHSL 0.5 0.7 0.7
```
You'll see a panel appear as you type showing the current errors in
the file (which are inevitable when you're still typing the code).
When it disappears, it means the object compiled successfully.
Flip your object over and you should see that it's changed
to a lovely shade of blue.

> Rumpus is structured as an "Entity Component System".
Entities are all of the distinct physical objects in the world, like the
one you just created.
Components are the properties that define how an entity looks and behaves,
like myColor in the example above, or myShape, or mySize, etc.
Systems take entities and their components and make them go,
performing tasks like Rendering, Physics, and Sound.
In the current version, Systems are "behind the scenes",
but you'll eventually be able to modify and build them too.

### Creating Entities

You can create entities in two ways: by grabbing them out of
the Object Bouquet, or by creating them programmatically.
To create a new entity via code, change the start function to:
```
start :: Start
start = do
    spawnChild $ do
        myColor      ==> colorHSL 0.5 0.7 0.7
        myShape      ==> Sphere
        myPose       ==> position (V3 0 1 0)
        mySize       ==> 0.1
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

### Giving Life

You can give life to our entity in a variety of ways.
The swiss-army knife of vivification is the `myUpdate` component;
change your start function to:
```
start :: Start
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
`myUpdate` is called each frame (90hz in the Vive) and can thus
be used to animate your object in whatever way you want.

### Multiple Children

You can call spawnChild as many times as you like.
Let's create a field of dreams:
```
start :: Start
start = do
    forM_ [0..99] $ \i -> do
        let x = (fromIntegral (i `mod` 10) - 5) * 0.1
            z = (fromIntegral (i `div` 10) - 5) * 0.1
        spawnChild $ do
            myColor      ==> colorHSL 0.5 0.7 0.7
            myShape      ==> Sphere
            myPose       ==> position (V3 0 1 0)
            mySize       ==> 0.05
            myUpdate     ==> do
                t <- getNow
                let t2 = t + fromIntegral i
                setPosition (V3 x (sin t2 * 0.05 + 0.5) (z - 0.5))
                setColor (colorHSL (sin (t2/2)) 0.7 0.7)
```

### Adding Knobs
You can parameterize your object by adding Knobs.
To create a simple knob, call `addKnob` with a name, a range of values and an
initial value.
Knobs will appear along the sides of the code slab.
You can read the value of the knob in your myUpdate function using readKnob.
Change your start function to add speed and height knobs:
```
start :: Start
start = do
    hueKnob    <- addKnob "Hues"   (Linear 0.1 1) 1
    heightKnob <- addKnob "Height" (Linear 0.1 10) 2

    forM_ [0..99] $ \i -> do
        let x = (fromIntegral (i `mod` 10) - 5) * 0.1
            z = (fromIntegral (i `div` 10) - 5) * 0.1
        spawnChild $ do
            myColor      ==> colorHSL 0.5 0.7 0.7
            myShape      ==> Sphere
            myPose       ==> position (V3 0 1 0)
            mySize       ==> 0.05
            myUpdate     ==> do
                hues   <- readKnob hueKnob
                height <- readKnob heightKnob
                t <- getNow
                let t2 = t + fromIntegral i
                setPosition (V3 x (sin t2 * 0.05 * height + 0.5) (z - 0.5))
                setColor (colorHSL (sin (t2/2) * hues) 0.7 0.7)
```

Knobs are cool because their values are saved as persistent state,
which means they are also sent across in multiplayer.

### Interaction and Physics
To create your own interactive objects, your object must have
a myBody component.

Adding a body adds an entity to the physics simulation so it can
react to gravity, collisions, or objects passing through it (like your hand).

To spawn a basic physics object, try a start function like this:
```
start :: Start
start = do
    spawnChild $ do
        myColor      ==> colorHSL 0.5 0.7 0.7
        myShape      ==> Cube
        myPose       ==> position (V3 0 1 0)
        mySize       ==> 0.1
        myBody       ==> Physical
```
The object will fall into the void. Try pulling out a Platform
object for it to land on.

There are 3 kinds of physics body you can request in `myBody`:
* `Physical` creates a "real-world" object that can be picked up, thrown,
    knocked around, etc.
    You can change these objects' Mass, Restitution, and Gravity
    using myMass, myRestitution, and myGravity
    (by default, they'll experience an Earth gravity of 9.8m/s^2)
* `Animated` creates a physics object that you animate
    yourself (e.g. with `myUpdate`).
    It is still a solid object, so it will knock other Physical objects around,
    but has effectively infinite mass and thus won't be affected by them.
    This corresponds to "Kinematic" in Bullet Physics.
* `Detector` creates an ethereal physics object that detects when
    objects are intersecting it but passes through them like a ghost.
    This corresponds to "NoContactResponse" in Bullet Physics.

### Collisions

To react when the object hits another object (including your hands),
use the myCollisionBegan, myCollisionContinues, and myCollisionEnded
components.
For example, to change color every time we touch something:
```
start :: Start
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
Adding a physics body also makes myPose refer to absolute rather than
relative coordinates.


### Attachment
You can attach physics bodies to one another with attachEntity like so:
```
start :: Start
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
The second argument to `attachEntity` is the pose offset at which
the entity should be attached.

### Dragging
Entities with myBody set will receive myDragBegan, myDragContinues,
and myDragEnded events when dragged by the hand controllers.
You can use this to implement interactive objects.
If you'd like to handle dragging yourself rather than using the
built-in behavior of attaching entities to the hand controllers when
they are dragged, you can set `myDragOverride ==> True` on your entity.

```
start :: Start
start = do
    childID <- spawnChild $ do
        myColor          ==> colorHSL 0.5 0.7 0.7
        myShape          ==> Cube
        myPose           ==> position (V3 0 1 0)
        mySize           ==> 0.1
        myBody           ==> Animated
        myDragOverride ==> True
        myDragContinues ==> do

            setColor (colorHSL x 0.8 0.8)
    attachEntity childID (position (V3 0 0.5 0))
```

### Teleportation
You can mark any physics object as "Teleportable" to
allow yourself to teleport on top of it using the Grip (side)
buttons on the Vive controller.
```
start :: Start
start = do
    spawnChild $ do
        myColor      ==> colorHSL 0.5 0.7 0.7
        myShape      ==> Cube
        myPose       ==> position (V3 0 0 -1)
        mySize       ==> 0.1
        myBodyFlags  ==> [Teleportable]
```

### Synthesis
Every entity in Rumpus can have a synthesizer attached to it.
The sound will radiate from that entity's position in the world.

Synthesizers are defined using the Pure Data DSP language.
To start with, you can use one of the included synthesizers.

Try this:
```
majorScale = map (+56) [0,2,4,7,9]

start :: Start
start = do
    setSynthPatch "Fountain.pd"

    myCollisionBegan ==> \_ _ -> do
        note <- randomFrom majorScale
        sendSynth "note" (realToFrac note)
```
The code slab will now play a random note from the major scale
each time it is touched.

### Polyphonic Synth Patches

This works well for long-running synthesis patches, but is inefficient
for objects with many transient instances or sounds
(for example, we want to spawn possibly hundreds of Note objects,
but we don't want to spawn hundreds of individual "Note" synthesizers)

Instead, we can use `acquirePolyPatch` like so:
```
majorScale = map (+56) [0,2,4,7,9]

start :: Start
start = do
    myCollisionBegan ==> \_ _ -> do
        note <- randomFrom majorScale
        acquirePolyPatch "Note.pd"
        sendSynth "note" (fromIntegral note)
        sendSynth "trigger" 1
```
This will create a bank of 8 Note.pd instances,
and assign them on demand to the most recent entity to call acquirePolyPatch.

### Animations
Coming soon

### State
Coming Soon

### Entity IDs, Contexts, Setters, Getters

Every `spawnEntity`/`spawnChild`/`spawnChildOf` call returns an `EntityID`.

You can use this EntityID with functions like `setEntityColor`
and `getEntityColor` to make changes.

If you want to make multiple changes to an entity, you can use the
`inEntity` function to make that more convenient:
```
start = do
    childID <- spawnChild $ myColor ==> colorHSL 0.5 0.5 0.8
    -- You can do this:
    setEntityColor childID (colorHSL 0.5 0.5 0.2)
    setEntityPose childID (colorHSL 0.5 0.5 0.2)
    -- Or this:
    inEntity childID $ do
        setColor (colorHSL 0.5 0.5 0.2)
```
You'll notice two ways to set values:
The `myFoo ==> Bar` syntax and the setFoo Bar functions.

The basic rule of thumb is that `myFoo ==> Bar` is used
when defining an object (i.e., within a spawnChild block),
and `setFoo Bar` is used to modify an already existing
object (e.g. in a `myUpdate` function).
For cases where no side effects are needed (or wanted),
`==>` can still be used.

This is a historical accident and will be fixed in a future version.
(The reason they both exist: `==>` is "side-effect free" and simply sets
a component value directly, which is desirable when
an object is being constructed, whereas the `set` functions trigger
necessary side effects on existing objects,
such as recreating physics bodies when their shape or size is changed)









































