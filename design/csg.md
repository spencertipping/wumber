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
+ **Cons**
  + Rendering is expensive: need to find boundaries first
  + Not a natural fit for sketch-style drawing
