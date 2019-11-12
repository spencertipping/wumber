# Development frontier
+ Sym/JIT
  + **Done:** Trivial AMD64 JIT for Sym (no compile analysis/optimization)
  + **Done:** Sym -> Isofn
  + Call back into Haskell/C
  + Piecewise/`cond`
  + Reverse-engineer instruction latency/throughput
  + Sym -> constraint isofn?
  + CSE and vectorization?
+ QA
  + **Done:** JIT tests
  + **Done:** JIT benchmarks
  + Dual contouring benchmarks
  + Compilation benchmarks
  + Refactor benchmark harness generally, enable named selection
+ DSL
  + Finalize constraint DSL
  + Figure out object model for common shapes, esp axis/angular symmetries
  + Mirror x/y/etc?
  + Boundary hinting
+ Constraints
  + Partition by inputs
  + Simplify linear subsystems
+ Dual contour
  + Accept/generate surface hints, e.g. by extrapolating from tangent planes
  + Incremental/progressive scanning (reuse already-computed data)
  + Prioritize bisections by expected error
  + Deduplicate function evaluations, possibly just with a `Map`
  + Cache boundaries to disk (requires fingerprinting for Sym)
  + Mesh of quads, not just lines
  + View contouring? i.e. dual contouring in 2D wrt the view matrix?
  + 4D meshing for animation/object-through-time collision detection
+ Shell
  + **Done:** JIT/iso-mesh inside shell process
  + **Done:** Model as pure data structure
  + LOD based on view matrix: incorporate "visually perceptible error" into DC
+ Parts
  + Iso-libraries for screws, beams, etc
  + Bearings
  + Axles, pulleys, belts
  + Use constraints to solve for belt path between pulleys


## Long-term
+ FEA prototype, incl auto-meshing
  + Anisotropic material properties specified by vector fields
  + Material property vectors for F-rep solid modeling
  + Fast approximation > slow detail; use this for structural optimization
+ EDA
  + Trace capacitance/impedance: integrate layout and design
  + Live-test hardware driver for unknown circuits
  + Fast approximation > slow detail; we can always export to SPICE
