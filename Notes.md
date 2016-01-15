

[x] Start functions
    Make cmpUpdate just refer to code.
    Editor should have a channel that it places new functions into to be consumed by rumpus.
    Likewise for start function.
    [ ] should have cmpStart that is run at beginning of frame when present, then deleted.
        (make sure start is called on the children before they're instantiated too!
        probably need to not spawn the entity til its code is ready)


Core
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


Features
- [ ] Instanced rendering
- [ ] Entity property-editor sliders for anything not easily directly-manipulable
- [x] Switch between edit and play modes (edit = no physics, reset initial positions)
        Would be fun to pause physics to inspect too.
- [ ] Resource pooling - rather than deleting resources, return them to a pool to be pulled out by new entities.
        Allocate a big chunk of these at startup.

Modules
- [ ] Animation component
- [ ] Text editor module
- [ ] Shader editor windows
- [ ] Haptics
- [ ] FFT module
- [ ] Chain link module

- [x] Music component
- [x] Key handling


- Build Pd patches physically. Remote control Pd in the same way as metapatching.
    UPDATE: I've got this working in pd-haskell/test/test-meta

- Extensible 
Need a better way to define component-systems.
E.g, would like a "Lifetime" component-system that makes a thing automatically expire.

No need to make these fully runtime, I guess, thanks to halive?

UPDATE: ok, I've got a sketch of this in Projects/extensible-systems/

- Script systems
"Script" system as parallel path. Component holds init func, which returns a state, and an update func which can update the state. Hold state in a Dynamic.


- Touchable audio
Route touchable audio through the vive touchpad such that it's audible from the outside!!
Build a pair of threads that just listen for a frequency + envelope + duty cycle message and transform it to pulses for the controller.

Demos:
Spherical piano
Piano room
Fractal Flower room
Drum machine


http://bulletphysics.org/Bullet/BulletFull/classbtConeTwistConstraint.html
http://www.vrinflux.com/newton-vr-physics-based-interaction-on-the-vive/
http://pomax.github.io/bezierjs/
http://www.continuousphysics.com/Bullet/BulletFull/classbtIDebugDraw.html


GetTrackedDeviceIndexForControllerRole
http://steamcommunity.com/app/358720/discussions/0/451848855009182860/


Keyboard API
http://steamcommunity.com/app/358720/discussions/0/481115363863637539/
