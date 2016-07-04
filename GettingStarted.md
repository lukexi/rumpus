# RUMPUS MANUAL

Ok, so you're ready to rump. Rumping is not for the faint-hearted;
nothing is finished, everything is subject to change. I don't think typing
letters into virtual screens in VR is a "good idea"*. But it's still a heck
of a good time and you'll probably have some fun.

> *Higher-level "VR native" languages are underway, but I wanted a
development environment and "assembly language" first, and Rumpus
is a snapshot of that process.

***Intro

### RUMPUS BASICS
When you boot up Rumpus you'll be in the **Scene Loader**.
Choose one of the orbs around you to visit that world.

Pick up objects by grabbing them with the trigger. Hold the grip buttons
to bring up the **Teleport Beam** - point it at platforms and release when it
turns green and starts to rumble to teleport to that location.

Press and hold the "App Button" (above the touchpad on your Vive controller)
on one hand to bring up the **Object Bouquet**. Pull objects out of the
bouquet with your other hand to bring them into the world. To destroy an
object, drag it into the center of the Bouquet and release.

You can import objects from other scenes by pressing "Import", then choosing
a scene, then grabbing one of its objects. It will be copied into your scene
so you're free to modify it without changing the original.

To return to the **Scene Loader**, click the green orb that appears at the
bottom of the wand when holding the App Button with your other hand.

You can create your own scene by clicking the "New Scene" orb in
the **Scene Loader**. Scenes are persistent, so you can build arrangements
and revist them later.

***

To learn to edit and create your own objects,
see _RUMPUS PROGRAMMING GUIDE_ below.

### RUMPUS MULTIPLAYER
This is currently a hack with many caveats. You shouldn't do this with
anyone you don't know, as you're going to be running arbitrary code that
can do absolutely anything. Better things are in the pipeline. But again,
it's extremely fun anyway and you should totally try it if you have
friends you can trust.

How it's done:
The first time you run Rumpus, it creates a folder in `My Documents/Rumpus`.
Inside that folder, you'll find a file called redirect.txt, and inside
that file, you'll see an example path like
`c:/Users/MY_USERNAME/Dropbox/MY_SHARED_FOLDER`.

Create a shared Dropbox folder with your friend (Rumpus doesn't know
anything about Dropbox, so you can also use any service that has
similar capabilities), and type the path to that folder into `redirect.txt`.
Have your friend do the same.

Launch Rumpus, and everything you do will now be synchronized to your
friend's computer. Creating, moving, and deleting objects, creating scenes,
and editing code will all be synced - usually within 100ms or less. This
should work with any number of players (provided you try not
to step on each others toes by e.g. editing the same file simultaneously),
and since it's just a Dropbox folder, it doesn't matter if everyone
is "online" when you're working; changes will be synced regardless and
your friends will see them next time they boot up Rumpus.

> Note: transient state like the physics simulation is not synced - that'll
be coming soon in the "real" Rumpus multiplayer implementation.

You don't have to do any coding to do the multiplayer; you can, for example,
use the Note, Playhead, Sampler and Verylogue objects to
collaboratively compose music.

Or, if you have befriended a code wizard, you can just hang out
with them and play with what they make.

Rumpus has no built-in voice chat yet, so I use Steam's voice chat.

To return to single-player mode or change who you're working with,
just edit or delete `redirect.txt`.
