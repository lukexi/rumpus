[ ] Fix touchpad support... can we detect the button yet?
[x] Fix hand ordering
[x] Try SteamVR keyboard again

[x] Fix OpenAL positioning

[ ] Enable multisampling
    https://www.opengl.org/wiki/Multisampling

[ ] See if calling swapBuffers every other frame fixes skips!
    (i.e., thinking that maybe it is because the vive is triggering us at 90hz 
    and that upsets something when blitting to the screen at 60hz.)

TONIGHT:
Let's build the music sequencer.

FIRST: Try the system out from inside. Fix egregious bugs.


What's needed?

[x] Note object that changes pitch based on height.
[x] Already have playhead - transparency would be nice but not essential.


Let's build it first, then see what we need to build it from scratch.
Note and playhead objects just need update function, scaling, ghost object designation.
Sketch out some ideas for how to interact with components.
E.g. Scale. Color. Shape. PdPatch. Mass.
    I like the idea of tools; choose the tool you want, reach into the object, use the touchpad.
    Clone tool: hold the object you want to clone in your left hand, then hit trigger with right hand to place.
Need a way to create script files, and assign them to objects.
Create a script by default with a name like NewScript1 when creating an object, then just clone the object?
    Clones inherit names from their parents, so the code will be inherited.

Spherical bouquet menus for choosing objects to duplicate

Bouquet menu... take this more literally : ) — have it pop out of your hand like a magician
The shapes could be on the end of stems and you pluck them out and a new one grows
U o E
\ | /
 \|/
  |
[hand]  


THEN:
Microphone stuff!
Instanced rendering would be awesome for FFT viz.
See if we can make that efficiently and nicely user-specifiable.

Core
- [x] Load scenes from folders, add their folder to the path

- [x] Start functions
    Make cmpUpdate just refer to code.
    Editor should have a channel that it places new functions into to be consumed by rumpus.
    Likewise for start function.
    [x] should have cmpStart that is run at beginning of frame when present, then deleted.
        (make sure start is called on the children before they're instantiated too!
        probably need to not spawn the entity til its code is ready)

- [x] Direct manipulation to create (initially) boxes and spheres
    - Use a retargetable animation to pull the object to the hand
- [x] Spawning instances
- [ ] Broadcasting events
    - Add a second channel or reuse the control events list with a 3rd Dynamic Entity Event type
    - How to make this extensible?
        - Dynamic is a good fit here, I think;
            use similar mailbox idea from cloudhaskell,
            onEvent events $ \MyEventType x -> frobWith x
            where onEvent does
            onEvent f = do
                events <- use wldEvents
                forM_ events $ \e ->
                    case fromDyn e of
                        Just r  -> f r
                        Nothing -> return ()
            This uses the cool trick where type inference flows backwards from the "f r" application
            to the fromDyn application.
- [x] Component files
    - [x] YAML file for scene definition, scripts in .hs files.
- [x] Destroying entities

Features
- [x] "Immovable" tag so we don't have to filter for the floor
- [ ] Instanced rendering
- [ ] Transparency
- [ ] Shadows
        - proof of concept in shadow-pal, integrate
- [ ] Entity property-editor sliders for anything not easily directly-manipulable
- [x] Switch between edit and play modes (edit = no physics, reset initial positions)
        Would be fun to pause physics to inspect too.

Efficiency
- [ ] Resource pooling - rather than deleting resources, return them to a pool to be pulled out by new entities.
        Allocate a big chunk of these at startup.
- [ ] Convert to just using matrices rather than poses

Modules
- [ ] Animation component
- [ ] Text editor module
- [ ] Shader editor windows
- [ ] Haptics
- [ ] FFT module
- [ ] Chain link module

- [x] Music component
- [x] Key handling
- [x] Script system
    "Script" system as parallel path. Component holds init func, which returns a state, and an update func which can update the state. Hold state in a Dynamic.


- Build Pd patches physically. Remote control Pd in the same way as metapatching.
    UPDATE: I've got this working in pd-haskell/test/test-meta

- Show the clipboard contents! Place it on your right wrist or something...

- [x] Extensible 
    Need a better way to define component-systems.
    E.g, would like a "Lifetime" component-system that makes a thing automatically expire.

    No need to make these fully runtime, I guess, thanks to halive?

    UPDATE: I've got a sketch of this in Projects/extensible-systems/

- [ ] Add texture library?
    https://community.renderman.pixar.com/article/114/library-pixar-one-twenty-eight.html


Demos:
- Spherical piano
- Piano room
- Fractal Flower room
- Drum machine
- Fireworks (!)
    Fire streamers from hands that explode when hitting a surface, all with positional sound
- Mandelbox
    https://vimeo.com/152795080
- Wind chimes
- Bow&Arrow

- Implement some of the examples from http://natureofcode.com/book/chapter-6-autonomous-agents/ !
    - In particular, agent simulations where we can change the step function as things are moving
        should be pretty damn rad.

- Cellular growth
    c.f. Andy Lomas https://vimeo.com/82989945

- Garage band mini based on sequencer idea. Simple sampler, simple synth.
    - Have done synth : D needs presets.
    - Simple sampler next!

- Touchable audio
    Route touchable audio through the vive touchpad such that it's audible from the outside!!
    Build a pair of threads that just listen for a frequency + envelope + duty cycle message and transform it to pulses for the controller.

- Multiplayer is *really important*, do it asap before system gets too big as it will be a lot of rote refactoring.

- Implement Bret's "geometric clock" example:
    spinning plane that hits a sphere collider which emits a pulse

Links:

http://bulletphysics.org/Bullet/BulletFull/classbtConeTwistConstraint.html
http://www.vrinflux.com/newton-vr-physics-based-interaction-on-the-vive/
http://pomax.github.io/bezierjs/
http://www.continuousphysics.com/Bullet/BulletFull/classbtIDebugDraw.html


Keyboard API
http://steamcommunity.com/app/358720/discussions/0/481115363863637539/



SHADOWS:
Recommends CSM as 2012 state of the art in simplicity and quality
http://www.opengl-tutorial.org/intermediate-tutorials/tutorial-16-shadow-mapping/

Recommends ESM as fast.
http://blog.kiteandlightning.la/variance-shadow-maps/

Implementing VSM
https://www.youtube.com/watch?v=mb7WuTDz5jw

http://codeflow.org/entries/2013/feb/15/soft-shadow-mapping/#variance-shadow-mapping-vsm

The Book: Real-Time Shadows ($69)
http://www.amazon.com/gp/product/1568814380

https://msdn.microsoft.com/en-us/library/windows/desktop/ee416324%28v=vs.85%29.aspx

http://learnopengl.com/#!Advanced-Lighting/Shadows/Shadow-Mapping

Generic spotlight shadows
https://www.youtube.com/watch?v=5rg1Kd_2TkQ

SSAO
http://john-chapman-graphics.blogspot.co.uk/2013/01/ssao-tutorial.html
http://john-chapman-graphics.blogspot.co.uk/p/demos.html
https://www.youtube.com/user/johnChapman

NETWORKING:
http://enet.bespin.org/Features.html

http://gameprogrammingpatterns.com/game-loop.html


http://graphics.cs.williams.edu/papers/TransparencyI3D16/
https://tel.github.io/posts/mutable_algorithms_in_immutable_languages_part_3/


Add Subhalive API based on StringBuffer so we don't need files at runtime anymore
See:
targetContents :: Maybe (StringBuffer, UTCTime)
https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc/GHC.html
https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc/StringBuffer.html#t:StringBuffer

NOTE: also reconsider the idea of a pure haskell markup file, interpreted by Subhalive.
Basically use a quasiquoter to capture haskell code as a string, which should play well with
external editors' syntax highlighting. 
(at first I thought this would enable using control structures in object defs, but that wouldn't be re-serializable)
Or, just add a different extension for the YAML, embed the Haskell code and tell Sublime that it is Haskell.