# Splines
There are a lot of things that don't make sense to me about how splines are
defined and used, although the math behind B-splines seems reasonable. From a
CAD perspective, though, NURBS seem awkward: how do you define objects in terms
of surface points and tangent planes? (Or does one ever want to do this, without
creating a plane of control points?)

The other big issue is how we get a volume function from a mesh of control
points. I suppose we could dot against the surface normals, but it doesn't seem
like a closed-form solution to me: we're doing an argmin over curved surfaces.


## Control points and constraints
I have to think we want the control points to mean something relevant to the way
we define constraints. As it is, I'm not sure they're very geometrically related
to the shape that comes out: they aren't on the surface.

I guess a bigger question/issue here is, what do we want splines for anyway? I
can see two use cases:

1. Elements without any sharp edges, e.g. to minimize fluid resistance
2. Elements under load: bent springs

As for control points, I think it's OK for them to be abstract. We'll probably
set them using two types of constraints:

1. Cross-section must provide X amount of flexural stiffness
2. Cross-section must fit within X bounds
