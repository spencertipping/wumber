# Constructive solid geometry
...and really, I mean "wumber supporting real solid objects".


## Pros/cons
+ **Pros**
  + Real volumes let us automesh for FEA
  + More complex/accurate geometries with less work, probably
  + Tool modeling with collision detection
  + I don't really know how to do it, so I'll learn something
  + Powerful, open-ended object algebra
  + Auto-fillet and auto-chamfer become possible
  + Better rendering
+ **Cons**
  + Objects are highly structured: less abstract stuff maybe?
  + It will take a while for me to figure out how to implement it
  + CSG is mostly about subtractive manufacturing; what about assembly-focused?
  + CSG isn't a substitute for weld simulation for FEA purposes
  + Solids create edge cases: abutting faces, inside-vs-outside, etc


## Representing solid objects
I _really like_ [colah's implicit CAD](https://github.com/colah/ImplicitCAD).
Let's go through the pros/cons of implicit geometries:

+ **Pros**
  + CSG basically for free
  + Instant point-in-object checking
  + Good excuse to use JIT
  + Mathematically robust: no edge cases
  + Arbitrary FEA meshing is easy
  + Chamfers and fillets are easy, I think
  + Interesting things like shape->shape interpolation are trivial
  + Bounding boxes can be inferred given a point in the object
  + Curves, splines, etc are trivial to handle
  + Cross sections are also trivial
+ **Cons**
  + Rendering is expensive: need to find boundaries first
  + Solutions are inexact because we don't have a minimum detail size; i.e.
    we're voxel-driven
+ **Neutrals**
  + We can leverage some symmetries symbolically, since we have the op-tree
    (other approaches also provide this)
  + Sketch-style drawing is done differently: lines have sides and we intersect
    stuff

I don't want to read too much into the inexact stuff. It's a feature in disguise
because we already know the level of detail we want as a viewer: it's our "skip
rendering for small objects heuristic that we currently do with bounding boxes.
Put differently, implicit gives us LOD scaling for free.

It's a dealbreaker if implicits are so slow that we can't iterate visually, but
I'm not convinced we'll have that problem. If we can calculate a robust hashcode
for an object, we can save its mesh/interior/etc to disk if we want to. I think
we get a lot of mileage from reusing data.

**Can we apply a view matrix to 3D objects while they're implicit?** I don't see
why not; if we can, then we can solve everything in 2D unless we need face
shading.


### Proof of concept
I want to figure out two things:

1. Whether my view-matrix idea will work at all
2. Whether we can solve for boundaries in realtime

(2) may prevent us from using the view-matrix trick because it demands ~30FPS
rendering.

I think we can get by with two test cases. One is just a single sphere and the
other is a grid of 10x10x10 spheres.
