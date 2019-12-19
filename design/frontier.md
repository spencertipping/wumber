# Development frontier
## Priority queue
+ Precision-variant BRep structure
+ Composite visualization in general
+ 3D object view
+ Live code iteration that doesn't segfault
+ EDA simulation + visualization
+ Structural FEA + visualization
+ New AMD64 JIT backend


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
  + Rewrite around re-entrant, progressive scanning
  + Use observed gradients and f-values to infer isosurface/face collisions
  + FEA mesh emitter: generalize to arbitrary-dimensional scanning?
  + Second derivatives to guide split points?
+ Shell
  + New architecture that prevents `hint` from segfaulting
  + Augment/rewrite Gloss so we can use real fonts/etc
+ Shell CLI
  + Build views programmatically, integrated with constraints


## Modeling
+ 2D
  + Render 2D within a 3D context, given a plane + UV coordinates
  + Diagrams/lines/graphs/etc?
+ Parts
  + Iso-libraries for screws, beams, etc
  + Bearings
  + Axles, pulleys, belts
  + Simulation: power transfer, axial load limits, angular momentum


## Backend
+ Distributed compute daemons
  + Complicated because we need to move code
  + Can we pre-reduce everything to `SymMath` or similar?
+ LOD-variant feature hinting
  + Calculate LOD by view prominence
  + How do we cache this if it's dynamic?
+ Assembler
  + Multi-branch assembly with conditionals
+ JIT
  + Reverse-engineer instruction latency/throughput
  + Benchmark reg/reg and reg/memory parallelization
+ Simulation
  + 4D meshing for animation/object-through-time collision detection
+ Meshing
  + Progressive solid meshing for FEA, driven by error gradient
