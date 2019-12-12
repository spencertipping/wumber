# Development frontier
## Priority queue
+ Live code iteration that doesn't segfault
+ EDA simulation + visualization
+ Structural FEA + visualization
+ Vector+gradient JIT
+ Symbolic differentiation


## Simulation
+ FEA prototype, incl auto-meshing
  + Anisotropic material properties specified by vector fields
  + Material property vectors for F-rep solid modeling
  + Fast approximation > slow detail; use this for structural optimization
+ EDA
  + Circuits as constraint systems to be minimized (proof of concept)
  + Trace capacitance/impedance: integrate layout and design
  + Live-test hardware driver for unknown circuits
  + LXI scope data collection?
  + Fast approximation > slow detail; we can always export to SPICE


## Frontend
+ Dual contour
  + Deduplicate function evaluations
  + Prioritize bisections by expected error
  + Accept/generate surface hints, e.g. by extrapolating from tangent planes
  + Detect errors by verifying surface normal against function gradient
  + More accurate edge rendering, particularly for nonaligned corners
  + Incremental/progressive scanning (reuse already-computed data)
  + Mesh to quads/prisms, not just lines
+ Shell
  + New architecture that prevents `hint` from segfaulting
  + LOD based on view matrix: incorporate "visually perceptible error" into DC
  + Augment/rewrite Gloss so we can use real fonts/etc
+ Shell CLI
  + Select variable(s) to be rendered
  + Build views programmatically, integrated with constraints
  + Export bulk constraint solutions


## Modeling
+ 2D
  + Render 2D within a 3D context, given a plane + UV coordinates
  + Support for text/labels?
  + Diagrams/lines/graphs/etc?
+ Parts
  + Iso-libraries for screws, beams, etc
  + Bearings
  + Axles, pulleys, belts
  + Simulation: power transfer, axial load limits, angular momentum


## Backend
+ LOD-variant feature hinting
  + Calculate LOD by view prominence
  + How do we cache this if it's dynamic?
+ Sym compiler/algebra
  + Symbolic derivatives
+ Assembler
  + Multi-branch assembly with conditionals
  + Constant vector + indexing
+ JIT
  + Vector output with parallel subterm reduction
  + Reverse-engineer instruction latency/throughput
  + Benchmark reg/reg and reg/memory parallelization
+ Simulation
  + 4D meshing for animation/object-through-time collision detection
+ Meshing
  + Progressive solid meshing for FEA, driven by error gradient
