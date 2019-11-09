# Development frontier
+ Sym/JIT
  + Port CVal logic to Sym
  + Trivial AMD64 JIT for Sym (no compile analysis/optimization)
  + Reverse-engineer instruction latency/throughput
  + Auto-vectorization using XMM sidecars?
+ DSL
  + Finalize constraint DSL
  + Figure out object model for common shapes, esp axis/angular symmetries
  + Mirror x/y/etc?
+ Constraints
  + Partition by inputs
  + Simplify linear subsystems
+ Dual contour
  + Cache boundaries to disk (requires hashable for Sym)
  + Mesh of quads, not just lines
  + LOD based on view matrix
+ Parts
  + Iso-libraries for screws, beams, etc


## Long-term
+ FEA prototype, incl auto-meshing
  + Anisotropic material properties specified by vector fields
  + Material property vectors for F-rep solid modeling
  + Fast approximation > slow detail; use this for structural optimization
+ EDA
  + Trace capacitance/impedance: integrate layout and design
  + Live-test hardware driver for unknown circuits
  + Fast approximation > slow detail; we can always export to SPICE
