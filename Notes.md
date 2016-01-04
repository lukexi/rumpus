

Core
- [ ] Direct manipulation to create (initially) boxes and spheres
- [ ] Spawning instances
- [ ] Broadcasting events
    - Add a second channel or reuse the control events list with a 3rd Entity Event type
    - How to make this extensible?
- [ ] Component files
    - [ ] YAML file for scene definition, scripts in .hs files.


Features
- [ ] Instanced rendering
- [ ] Switch between edit and play modes (edit = no physics, reset initial positions)
        Would be fun to pause physics to inspect too.
- [ ] Entity property-editor sliders for anything not easily directly-manipulable

Modules
- [ ] Animation component
- [ ] Text editor module
- [ ] Shader editor windows
- [ ] Haptics
- [ ] FFT module
- [ ] Chain link module

- [x] Music component
- [x] Key handling

Need a better way to define component-systems.
E.g, would like a "Lifetime" component-system that makes a thing automatically expire.

No need to make these fully runtime, I guess, thanks to halive?


"Script" system as parallel path. Component holds init func, which returns a state, and an update func which can update the state. Hold state in a Dynamic.


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