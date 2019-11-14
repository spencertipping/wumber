# Dual contouring for simulation
Dual contouring selectively subdivides and extrapolates to identify vertices. If
we extend to 4D with time, then vertices-in-time represent changes in topology
that may indicate colliding or diverging objects. This is more efficient than
time-stepping because DC is expanding only the nodes that (1) are non-planar,
and (2) contain interesting detail.

I don't believe this also applies to causal structures. The reason is that we
don't have two known endpoints to work with; the future depends on the present
(and derivatives). DC doesn't give us any leverage against the
integrate-to-get-next problem.
