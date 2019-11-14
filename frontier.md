# Development frontier
## Frontend
+ Dual contour
  + Prioritize bisections by expected error
  + Accept/generate surface hints, e.g. by extrapolating from tangent planes
  + Incremental/progressive scanning (reuse already-computed data)
  + Cache boundaries to disk (requires fingerprinting for Sym)
  + Mesh of quads, not just lines
  + View contouring? i.e. dual contouring in 2D wrt the view matrix?
  + Deduplicate function evaluations, possibly just with a `Map`
+ Shell
  + **Done:** JIT/iso-mesh inside shell process
  + **Done:** Model as pure data structure
  + LOD based on view matrix: incorporate "visually perceptible error" into DC
  + Restrict DC to small bounds, possibly integrated with model
+ Shell CLI
  + Select variable(s) to be rendered
  + Build views programmatically, integrated with constraints
  + Export bulk constraint solutions


## Backend
+ Sym API
  + **Done:** Sym -> Isofn
  + Call back into Haskell/C
  + Piecewise/`cond`
  + Sym -> constraint isofn?
+ Sym compiler
  + Algebraic simplification
+ JIT
  + **Done:** Trivial AMD64 JIT for Sym (no compile analysis/optimization)
  + Reverse-engineer instruction latency/throughput
  + Register allocation
  + Generate processor-optimized code
  + Auto-vectorization
+ QA
  + **Done:** JIT tests
  + **Done:** JIT benchmarks
  + **Done:** Dual contouring benchmarks
  + **Done but broken:** Hint load benchmarks
  + Refactor benchmark harness generally, enable named selection
+ Constraints
  + Partition by inputs
  + Simplify linear subsystems
+ Simulation
  + 4D meshing for animation/object-through-time collision detection
    + Collisions happen only at the `t` coordinates of 4D vertices


## Modeling
+ Parts
  + Iso-libraries for screws, beams, etc
  + Bearings
  + Axles, pulleys, belts
  + Use constraints to solve for belt path between pulleys
+ DSL
  + Finalize constraint DSL
  + Figure out object model for common shapes, esp axis/angular symmetries
  + Mirror x/y/etc?
  + Boundary hinting


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
