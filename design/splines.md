# Splines
There are a lot of things that don't make sense to me about how splines are
defined and used, although the math behind B-splines seems reasonable. From a
CAD perspective, though, NURBS seem awkward: how do you define objects in terms
of surface points and tangent planes? (Or does one ever want to do this, without
creating a plane of control points?)

The other big issue is how we get a volume function from a mesh of control
points. I suppose we could dot against the surface normals, but it doesn't seem
like a closed-form solution to me: we're doing an argmin over curved surfaces.
