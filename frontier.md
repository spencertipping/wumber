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
  + View contouring? i.e. dual contouring in 2D wrt the view matrix?
+ Shell
  + LOD based on view matrix: incorporate "visually perceptible error" into DC
  + Restrict DC to small bounds, possibly integrated with model
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
+ Object model
  + Parts as objects, not just implicit fns (this way we get constraints)
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
  + DC isn't sufficient for realtime modeling: we need boundary/corner hints
  + LOD-variant feature hinting
  + Calculate LOD by view prominence
+ Sym compiler/algebra
  + `ifpos` conditional instead of `upper`/`lower`
  + Cache `Binary` encodings for fast deduplication within `JITIR`
  + Symbolic derivatives
  + Algebraic simplification of sign expressions?
  + BVH primitives?
+ Isosurfaces
  + Bounding-box metadata as part of the function spec
  + BVH for CSG
+ Assembler
  + Multi-branch assembly with conditionals
  + Constant vector + indexing
+ JIT
  + Flexible register allocation (not all threads need extra registers)
  + Reverse-engineer instruction latency/throughput
  + Preload operands into registers while ALUs are busy
    + Model processor ports at some level
+ JITIR
  + Full `Sym` deduplication
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
