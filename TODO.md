# TODO
## High-level
1. Use implicit modeling for the backend
2. Use numerical constraints (which are also implicits) to specify the model
3. Ad-hoc meshing, possibly cached to disk
4. Provide an interactive shell on the terminal, linked to the view

[Full development strategy.](design/strategy.md)


## CSG
+ Tool path profiles
+ Turning/milling operations with drive code?
+ Chamfers and fillets?


## Sketch
+ Angled go-until things (e.g. "30° slope, dx = 5")
+ Compound `j` instructions
+ No more `replicateM_`; we want things like mirror, screw, extrude, etc (push
  inductive state management into the replicator)
+ Use less state; we want relative forks to be easy to write
  + e.g. chains like `jz (-2); rz 90; nema_17`
+ Waypoints/markers we can refer back to?


## Library
+ Machine bolt profiles, threading macros
+ NEMA stepper models
+ Arduino models
+ PCB elements
+ Prototyping breadboards
+ Common ball bearings
+ TGP shafts
+ Dimensional lumber
+ Material properties
  + Wood
  + A36 steel
  + Hardened steel
  + Aluminum


## FEA
+ Mesh to quads
+ Nonuniform/anisotropic material definitions (e.g. wood with knots)
+ Re-mesh to accommodate sharp force-distribution gradients
+ FEM for electronics
+ FEM for magnetics


## Rendering for development
+ Focus on specific elements
+ Focal plane
+ Snap view to align with specific things
+ CLI search
+ Face shading?
+ Rulers/guides for display
  + Rulers should exist both for manually-specified and solved constraints
  + Relative importance and screen-space tradeoffs: show what matters
  + Do 3D projection up front, then layout the OSD like a 2D system


## Export for manufacturing
+ Export to STL or similar
+ Export to cross-section PDFs or images
  + BigPrint-style image tiling?


## CNC
+ Generate toolpaths and tool geometry constraints
  + e.g. 1/2"x2" endmill, 4"x10" work envelope
  + Tolerances?


## Notes
CSG needs to happen early if we do it at all. It puts large mathematical demands
on objects that we don't have otherwise.


### Shell
[Full design here.](design/shell.md)


### JIT
JIT may or may not be worth it. It has two performance advantages: (1) we can
get rid of Haskell's polymorphic dispatch (50% faster, maybe); and (2) we can
use AVX for certain parts of the code -- but (2) isn't straightforward; we still
have a lot of scalar logic.

I don't think JIT makes sense out of the gate.

At some point down the line I'd like to try making an auto-vectorizer that finds
parallel expressions and builds an AVX pipeline to evaluate them. Processors
only barely make this type of thing worthwhile, though -- and many don't support
AVX.


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

The dumb option is to have the user manually define independent variables that
are constrained to other things. So the system would be fully specified when
those independent variables have fixed values. For example, we might say "this
is a drive axis" and step it by 1° increments, re-solving the other variables in
the system at each point.

(If we do things like this, we may want JIT after all.)

We can also verify that the model converges to a single state, and maybe
complain if it doesn't. Independent variable values should fully specify the
system.


### Implicit representation
POV-ray does this really well: isosurfaces are specified by a bounding box,
numerical accuracy, and max-gradient. That seems like a perfectly reasonable
representation for us to use.

If we know the bounding box, that means we can ray-cast to detect intersections
from the viewport. That, in turn, gives us a fast way to translate mouse actions
into 3D space. (To justify why this is feasible: if iso-scanning involves 10k
rays and takes less than five minutes, shooting one ray for mouse interactions
will easily fit into our 30ms realtime budget.)

...and again, we can always just cache a boundary for an object. The real
leverage of implicits is that they let us automate meshing, e.g. for
iteratively-meshed FEA.


### Boundary representation
It's worth getting some mileage from boundaries when we have them, which we will
for basic shapes like cubes. If we trust our boundaries, we can apply some
optimizations to CSG operations: the resulting form's boundary always consists
of elements from the originals and/or their split intersections.

...which I think means we just need two functions:

```haskell
split_triangle :: Triangle -> Triangle -> [Triangle]
split_line     :: Line     -> Line     -> [Line]
```

I guess we also want [BVH][bvh] if this is going to be remotely efficient. We'll
likely want it anyway to detect collisions faster than pure iso-intersection.

[bvh]: https://en.wikipedia.org/wiki/Bounding_volume_hierarchy
