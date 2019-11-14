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
awkward to update everything in a priority queue when we learn new things. We
can get around this a little if we make the rule that nobody can decrease error.
Then it's "modify N head elements", not "modify every element" -- and I think
the overhead is justifiable.
