# Modeling DSL
The shell needs to iteratively reload your model as you're editing it. This
means we need a fast pipeline between source and rendered stuff. There are a few
optimization points:

1. The `hint` compiler (write code that compiles quickly)
2. Isofunctions produced by your code (JIT to remove interpreter overhead)
3. Dual contouring (variable resolution driven by view matrix)

I'm not sure whether it's possible to make an argument like "the view can only
accommodate N lines, so we only have to do f(N) work" -- but if we can, that
would be awesome.

A modeling DSL would be targeted at the first two points above. The idea is that
it's a declarative numerical structure that compiles quickly, has low library
overhead, and could be compiled quickly using JIT.
