# Contouring
Right now I'm using a stock dual contouring algorithm to generate function
boundaries. It works very well for large features and falls apart on small,
sharp objects. This makes sense; the normal dual-contouring approach is to
bisect when we get cells whose edges -- indicated by corners -- cross the shape.
But some shape features will fall between cell corners.

We can get around this particular case in a couple of ways:

1. Infer a possible intersection by looking at the F-value and gradients, and
   assuming an upper bound on the function's rate of change
2. Use an expected-error metric to prioritize bisections

(2) generalizes (1), so let's see if it's feasible.


## Prioritized bisection and expected error
We're basically turning a depth-first expansion into guided BFS. We'd have a
priority queue of nodes sorted by error/importance. The queue overhead shouldn't
be too high (for a model of any complexity it likely won't be, relative to
isofn evaluation).

We need to be able to infer error nonlocally: a cell _near_ some error should be
able to indicate expected error in another location. This suggests that our
error is a distribution we'll discover as we go, which might break stuff. It's
awkward to update everything in a priority queue when we learn new things.

...so rather than have any sort of continuous distribution, maybe we just
attribute error directly to un-bisected cells and maintain them in a spatial
index. Then updates are _O(log n)_: find by spatial index, unheap, reheap.

I also like this idea because it lets the user iteratively refine the mesh.


## Bisection vs arbitrary splits
The explore-the-space problem doesn't demand that our splits bisect anything;
they could be chosen to maximize expected unknown at the split point. If we
apply the right distributive property to the line-tracing step (as we do now), I
think it should also work fine without even splits.

(Verified: nothing breaks catastrophically, although the model looks strange if
bisections are sufficiently biased.)


## Progressive contouring
Or, how do we rapidly update a potentially huge list of lines we've obtained
from tracing? ...actually, let's skip this for now. Our current trace overhead
is about 1μs/line, which isn't much (and we can't easily improve it).

Instead, let's focus on the tree-exploration end. This is pretty simple if we
just use local error; we calculate it once per cell, determine a threshold, and
expand. If we're time-sensitive we might run the highest-error cells for n
milliseconds, then stop and iterate.

If we're using the view matrix to calculate apparent error, then we'll want to
be able to quickly re-traverse and redo calculations, then sort and split the
most important ones. This is going to require our nodes to store the max error
in their children so we can skip whole sections at a time -- and that means the
view-matrix step is applied at the last moment, as a scaling factor.

```
unexpanded nodes in tree
  >> heapify by vm-scaled size * intrinsic error
  >> pull+split until deadline (and heapify children)
```
