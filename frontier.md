# Development frontier
## Frontend
+ Dual contour
  + Prioritize bisections by expected error
  + Accept/generate surface hints, e.g. by extrapolating from tangent planes
  + Incremental/progressive scanning (reuse already-computed data)
  + Mesh of quads, not just lines
  + View contouring? i.e. dual contouring in 2D wrt the view matrix?
  + Deduplicate function evaluations
+ Shell
  + LOD based on view matrix: incorporate "visually perceptible error" into DC
  + Restrict DC to small bounds, possibly integrated with model
+ Shell CLI
  + Select variable(s) to be rendered
  + Build views programmatically, integrated with constraints
  + Export bulk constraint solutions


## Modeling
+ General
  + Parts as objects, not just implicit fns (this way we get constraints)
  + NURBS or some sort of splines
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
+ Sym API
  + Call back into Haskell/C
  + Piecewise/`cond`
  + Sym -> constraint isofn?
+ Sym compiler
  + Algebraic simplification
  + Alias local quantities (SSA variables)
+ JIT
  + Reverse-engineer instruction latency/throughput
  + Register allocation
  + Generate processor-optimized code
  + Auto-vectorization
+ Constraints
  + Partition by inputs
  + Simplify linear subsystems
+ Simulation
  + 4D meshing for animation/object-through-time collision detection
    + Collisions happen only at the `t` coordinates of 4D vertices


## Long-term
+ FEA prototype, incl auto-meshing
  + Anisotropic material properties specified by vector fields
  + Material property vectors for F-rep solid modeling
  + Fast approximation > slow detail; use this for structural optimization
+ EDA
  + Trace capacitance/impedance: integrate layout and design
  + Live-test hardware driver for unknown circuits
  + Fast approximation > slow detail; we can always export to SPICE
  + `n+1`-D dual contouring for implicit time-stepped FEA
  + Circuits as constraint systems to be minimized
