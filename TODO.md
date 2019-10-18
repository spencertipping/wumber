# TODO
## CSG
+ Tool path profiles
+ Turning/milling operations with drive code?
+ Chamfers and fillets?


## Sketch
+ Angled go-until things (e.g. "30° slope, dx = 5")
+ Compound `j` instructions
+ `replicateM_` in designs isn't good; we want structured replicators
+ Use less state; we want relative forks to be easy to write
  + e.g. chains like `jz (-2); rz 90; nema_17`
+ Waypoints/markers we can refer back to?


## Library
+ Machine bolt profiles, threading macros
+ NEMA stepper models
+ Arduino models
+ Common ball bearings
+ TGP shafts
+ Dimensional lumber


## Rendering
+ Focus on specific elements
+ Focal plane
+ Snap view to align with specific things
+ CLI search
+ Face shading?
+ Rulers/guides for display


## Notes
CSG needs to happen early if we do it at all. It puts large mathematical demands
on objects that we don't have otherwise.


### CSG
This is going to be difficult because our object model isn't built to support
it. I've put everything together assuming that we're doing a fairly direct
render-to-screen.

For example: we don't have a "strict contains** operator, so we don't know
whether a point is inside an object. In fact, we barely have objects as such;
they're just drawings of objects.

I'm also not convinced we want to get on the auto-chamfer/fillet bandwagon. It
isn't that odious to specify them by hand, at least so far. Maybe it would be if
we had generated/replicated edges or something.

**The big question:** should wumber understand what solid objects are? Do _I_
understand what solid objects are, mathematically speaking?
