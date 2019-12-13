# Progressive dual contouring
First, let's integrate tree construction and scanning into a single layer. I
think this makes it possible to derive a split operation in terms of scans --
e.g. split and create a new line segment or split and bifurcate. (The downside:
it's complicated because we may not be scanning a single contiguous shape.)

Second, let's go ahead and fix up the duplicated-evaluation problem.

Third, more cleverness with the surface normals we end up observing. For
example, can we use them (and the inferred surface points) to prioritize new
splits and/or allocate subcells?


## Monotonic priority queueing
We need a priority structure with the property that `P(child) < P(parent)` in
every case. Then we can use Haskell laziness without the risk of inadvertently
forcing thunks. (I guess we could also maintain a regular priority queue and
just traverse that when we want leaves.)
