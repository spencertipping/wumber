# TODO
## High-level
1. Use implicit modeling for the backend
2. Use numerical constraints (which are also implicits) to specify the model
3. Use JIT for both constraints and implicits generally
4. Ad-hoc meshing, possibly cached to disk


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
+ Material properties
  + Wood
  + A36 steel
  + Hardened steel
  + Aluminum


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
[Full design here.](design/csg.md)

This is going to be difficult because our object model isn't built to support
it. I've put everything together assuming that we're doing a fairly direct
render-to-screen.

For example: we don't have a "strict contains" operator, so we don't know
whether a point is inside an object. In fact, we barely have objects as such;
they're just drawings of objects.

I'm also not convinced we want to get on the auto-chamfer/fillet bandwagon. It
isn't that odious to specify them by hand, at least so far. Maybe it would be if
we had generated/replicated edges or something.

**The big question:** should wumber understand what solid objects are? Do _I_
understand what solid objects are, mathematically speaking? (Update: yes and
yes. They're volume functions with implicit boundaries.)


### Constraints
[Full design here.](design/constraints.md)

I don't want to JIT constraints initially; we're unlikely to spend nearly as
much time solving them as we do scanning iso forms. I also want it to be easier
to build higher-order constraints and quickly extend the DSL.

We can detect under/overconstrained models by looking at the cost function
derivatives. Local minimum at nonzero == overconstrained, zero-derivative at
cost=0 == underconstrained.

What do we do with underconstrained systems? We want to provide a function that
takes an independent variable and produces new values, but (1) do we know the
dimensionality of underspecified constraints; and (2) is there a simple way to
map parameter values to outputs? Can we reliably trace the outline of all forms
that result in linear paths?
