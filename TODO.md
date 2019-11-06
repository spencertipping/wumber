# TODO
## High-level
1. [Self-contained modeling DSL](design/model-dsl.md)
2. Provide an interactive shell on the terminal, linked to the view


## CSG
+ Tool path profile functions
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
+ Export to Gerber for EDA
+ Export to cross-section PDFs or images
  + BigPrint-style image tiling?


## CNC
+ Generate toolpaths and tool geometry constraints
  + e.g. 1/2"x2" endmill, 4"x10" work envelope
  + Tolerances?


## Notes
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

**Update:** I'm using criterion to measure `sphere` and `distance` calls in the
iso example, loaded by hint. Here are the outputs:

```
benchmarking sphere
time                 370.3 ns   (364.8 ns .. 377.0 ns)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 368.8 ns   (362.6 ns .. 378.9 ns)
std dev              24.44 ns   (17.45 ns .. 39.04 ns)
variance introduced by outliers: 79% (severely inflated)

benchmarking distance
time                 188.5 ns   (152.7 ns .. 232.7 ns)
                     0.856 R²   (0.796 R² .. 0.996 R²)
mean                 162.2 ns   (154.4 ns .. 188.6 ns)
std dev              41.16 ns   (14.99 ns .. 87.96 ns)
variance introduced by outliers: 99% (severely inflated)
```

There's _no way_ it takes this long to do the math required for this. I made a
benchmark that compares hand-coded and vector functions:

```
benchmarking handcoded sphere
time                 11.16 ns   (11.02 ns .. 11.29 ns)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 11.17 ns   (10.98 ns .. 11.65 ns)
std dev              1.023 ns   (309.8 ps .. 1.918 ns)
variance introduced by outliers: 91% (severely inflated)

benchmarking vector sphere
time                 147.2 ns   (145.7 ns .. 149.5 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 148.3 ns   (146.2 ns .. 151.8 ns)
std dev              8.930 ns   (6.462 ns .. 12.57 ns)
variance introduced by outliers: 77% (severely inflated)
```

So it isn't interpreter overhead, it's vector library overhead -- and it's
massive, about 15x. This easily justifies some type of JIT.

**Update update:** `cube` is 75x slower interpreted than compiled (1.47μs vs
20ns). Absolutely horrible.
