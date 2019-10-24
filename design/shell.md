# Wumber shell
I want wumber to work well as an interactive CAD system, even though it's
text-based. We get some of this with the `inotify` compiler hook, but there's
more to it. Specifically:

+ Focus on subcomponents
+ Focus on cross-sections
+ Modify DoFs
+ Search for things
+ Identify degrees of freedom


## Set DoF values
This is fast _iff_ these DoFs don't modify the boundaries of any implicits. This
corresponds to modifying solid objects: if you have a machine with moving parts,
those DoFs will be fast because moving parts are affine transformations. But if
you're redoing CSG by moving holes around or altering dimensions, that's a
different solid and we'll need to calculate new boundaries.

I think this means we'll want some sort of armature-representation for implicit
objects; that way we can constrain their affine transformations independently
from their implicit functions.


## Cross-sections
Really I think this comes down to getting a cross-section per independent part,
at least for things like extrusions. These cross-sections would be "suggested
views" or "planes of interest" or something.

As for navigation, do we want an object tree with named things? I think if I
have a model of something, I'd want to browse it structurally. For example,
let's suppose I'm making a milling machine. I'd want to be able to focus on the
XY table assembly, the drive head, the Z axis slide, etc. Each of those would
have views relevant to those parts. The milling machine as a whole would
probably have side/front/etc views so we can verify that the head doesn't crash
into the table before we max out the Z axis.


## FEA
Accurate FEA requires some setup: a scenario is probably a file, or at least a
function. We don't want to commit to a full run just to preview the output,
though. This means we have some sort of LOD slider: interactive = inaccurate and
fast, offline = accurate and slow.

So FEA outputs are a data structure that implements `Binary` -- just like
computed boundaries are.

...and this type of cached representation requires some mechanism to invalidate;
that means our elements need to compute structural IDs.


### Falstad-style previews
I really really like the [Falstad circuit simulator][csim]: it shows you exactly
what you want to know, so you can get a lot of prototyping leverage before using
something beefier (and far more arcane) like SPICE.

Ideally we don't have a wall between these fast iterative previews and real
simulations; real simulations are just previews that don't happen in realtime.

[csim]: http://www.falstad.com/circuit/circuitjs.html


## Shell as editor
Tabled for the moment.

**Premise:** The shell can even be a part of the editing process if it has a way
to save its state. I don't want it to modify the original source, but it could
write an export file that contains things like new constraints. The user could
then migrate those into the design.

+ **Pros:**
  + The shell can suggest things based on context and/or feedback
  + The user is likely focused on the shell one way or another
  + Shorter turnaround time than recompiling a model
+ **Cons:**
  + The context is likely not to correspond to a lexical context from model
    source
  + The shell saves out to a sidecar file ... this is clunky
