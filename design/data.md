# Models as data for things
If I build a structure that's specified in constraint terms, odds are good that
I'll want to be able to work with the solved output as its own object. This is
more involved than just, "simplify these constraints and hand me a solution";
it's "solve my model and hand me a structure-as-data".

...all of which is probably fine, but I think this pushes us further towards
models-as-objects, and it means we're likely to care about object traversal
functions. We probably also need to think about how complex CSG gets reduced. Do
we rebuild those objects from DC outputs, emitting a new mesh composite?

I suspect all of this will be driven vertically by use case. I'm not sure any
generalization is going to capture the right stuff up front.
