# Contouring
Right now I'm using a stock dual contouring algorithm to generate function
boundaries. It works very well for large features and falls apart on small,
sharp objects. This makes sense; the normal dual-contouring approach is to
bisect when we get cells whose edges -- indicated by corners -- cross the shape.
But some shape features will fall between cell corners.

We can get around this particular case in a couple of ways:

1. Infer a possible intersection by looking at the F-value and gradients, and
   assuming an upper bound on the function's rate of change
2. 
