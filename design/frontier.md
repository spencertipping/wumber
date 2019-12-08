# Development frontier
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
  + LOD based on view matrix: incorporate "visually perceptible error" into DC
  + Augment/rewrite Gloss so we can use real fonts/etc
+ Shell CLI
  + Select variable(s) to be rendered
  + Build views programmatically, integrated with constraints
  + Export bulk constraint solutions


## Modeling
+ 2D
  + Simple iso-profile for 2D objects (dual contouring)
  + Constraints for layout
  + Render 2D within a 3D context, given a plane + UV coordinates
  + Support for text/labels?
  + Diagrams/lines/graphs/etc?
    + This is more like sketching than modeling per se
+ Parts
  + Iso-libraries for screws, beams, etc
  + Bearings
  + Axles, pulleys, belts
  + Use constraints to solve for belt path between pulleys
+ DSL
  + Finalize constraint DSL
  + Figure out object model for common shapes, esp axis/angular symmetries
  + Mirror x/y/etc?


## Backend
+ Boundary-aware CSG
  + LOD-variant feature hinting
  + Calculate LOD by view prominence
+ Sym compiler/algebra
  + Symbolic derivatives
  + Tree traversal for new Sym library
  + Lensing/traversal by profile?
  + ~~`AlgebraicFunctionable`~~ ... walk this back
  + Generalized normal form collection
+ Assembler
  + Multi-branch assembly with conditionals
  + Constant vector + indexing
+ JIT
  + Flexible register allocation (not all threads need extra registers)
  + Reverse-engineer instruction latency/throughput
  + Benchmark reg/reg and reg/memory parallelization
  + Preload operands into registers while ALUs are busy
    + Model processor ports at some level
+ Simulation
  + 4D meshing for animation/object-through-time collision detection
    + Collisions happen only at the `t` coordinates of 4D vertices
+ Meshing
  + Progressive solid meshing for FEA, driven by error gradient
    + Is FEA incrementally splittable this way? (Probably, since it's an
      integral equation)


## Long-term
+ FEA prototype, incl auto-meshing
  + Anisotropic material properties specified by vector fields
  + Material property vectors for F-rep solid modeling
  + Fast approximation > slow detail; use this for structural optimization
+ EDA
  + Trace capacitance/impedance: integrate layout and design
  + Live-test hardware driver for unknown circuits
  + Fast approximation > slow detail; we can always export to SPICE
  + Circuits as constraint systems to be minimized
