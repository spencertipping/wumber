# Development frontier
+ Sym/JIT
  + **Done:** Trivial AMD64 JIT for Sym (no compile analysis/optimization)
  + **Done:** Sym -> Isofn
  + Reverse-engineer instruction latency/throughput
  + Sym -> constraint isofn?
  + CSE and vectorization?
  + Auto-vectorization using XMM sidecars?
+ QA
  + **Done:** JIT tests
  + **Done:** JIT benchmarks
  + Dual contouring benchmarks
  + Compilation benchmarks
+ DSL
  + Finalize constraint DSL
  + Figure out object model for common shapes, esp axis/angular symmetries
  + Mirror x/y/etc?
  + Boundary hinting
+ Constraints
  + Partition by inputs
  + Simplify linear subsystems
+ Dual contour
  + Cache boundaries to disk (requires hashable for Sym)
  + More robust handling of near-edge cases (convergent normals or boundary hints)
  + Mesh of quads, not just lines
+ Parts
  + Iso-libraries for screws, beams, etc
  + Bearings
  + Axles, pulleys, belts
  + Use constraints to solve for belt path between pulleys
+ Shell
  + JIT/iso-mesh inside shell process
  + Model as pure data structure
  + LOD based on view matrix


## Long-term
+ FEA prototype, incl auto-meshing
  + Anisotropic material properties specified by vector fields
  + Material property vectors for F-rep solid modeling
  + Fast approximation > slow detail; use this for structural optimization
+ EDA
  + Trace capacitance/impedance: integrate layout and design
  + Live-test hardware driver for unknown circuits
  + Fast approximation > slow detail; we can always export to SPICE
