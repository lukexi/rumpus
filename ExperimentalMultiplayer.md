# Rumpus Multiplayer (experimental++)
This is a hack with caveats: you shouldn't do this with
anyone you don't know, as you're going to be running arbitrary code that
can do absolutely anything. A fluid, effortless multiplayer experience is 
in the pipeline! 
But it's extremely fun anyway and you should totally try it if you have
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
