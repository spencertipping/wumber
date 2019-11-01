# Development frontier
+ **Possible but not yet worthwhile:** JIT for F-rep (SSE4, not AVX)
  + Complicated by operations that aren't implemented in hardware
  + Algorithm improvements first
+ FEA prototype, incl auto-meshing
  + Anisotropic material properties specified by vector fields
  + Material property vectors for F-rep solid modeling
  + Fast approximation > slow detail; use this for structural optimization
+ EDA
  + Trace capacitance/impedance: integrate layout and design
  + Live-test hardware driver for unknown circuits
  + Fast approximation > slow detail; we can always export to SPICE
