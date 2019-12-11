# Wumber: CAD in Haskell
[![build status](https://travis-ci.org/spencertipping/wumber.svg?branch=master)](https://travis-ci.org/spencertipping/wumber)

**Work in progress: don't trust CI output**

Wumber will be a programmatic CAD library for Haskell. The focus is on
multidomain modeling: you describe the thing you're designing as Haskell data
structures, then have your model implement Wumber typeclasses to project it into
views, FEA simulations, constraint spaces, etc. My goal is to be able to have
simulations that cross domains: e.g. simulate mechanical and thermal output for
a given workload with respect to the microcontroller pins that drive a motor
controller.

From a mechanical engineering perspective, Wumber takes a lot from [colah's
ImplicitCAD][ic] and [SolveSpace][ss]: it uses [F-Rep][fr] with [dual
contouring][dc] to compute boundary meshes, and both isofunctions and constraint
sets are described with [symbolic math][sym] that provides [automatic
differentiation][der] and backs into [JIT-compiled functions][jit] for numerical
steps.

Wumber is designed to be fast enough for interactive use, even for large
designs. Expensive steps are [cached to disk][disk] for reuse, and Wumber
supports live code reloading with [hint][hint] so you can iterate without
running `stack build`. The JIT compiler currently produces code that runs 5-30x
faster than compiled GHC (I assume because it avoids all memory allocation and
function calls) -- and that's without any support for SSE vector intrinsics or
AVX.

More importantly than interaction, though, I want Wumber to scale well enough
that you can use it for computational optimization. For example, I'd like to be
able to have it derive a part that satisfies a series of mechanical load
specifications and minimizes manufacturing cost or weight, based on iterated FEA
and process simulation.

+ [Dev channel](https://dev.spencertipping.com/channel/wumber)
+ [What I'm reading to build this.](design/reading.md)
+ [Here's what I'm working on.](design/frontier.md)


## What Wumber isn't
Like most things I write, this project is pure [dogfood][dog] so it's unlikely
to have a number of things people might care about:

1. An easy-to-use, accessible frontend like [OpenSCAD][oscad]
2. Any sort of GUI beyond previewing
3. AVX support (my main compute server runs pre-AVX Xeons)
4. LLVM support (I wanted practice writing JIT compilers)
5. G-code generation (I plan to implement custom CNC hardware)

If you want features I don't have planned, I highly recommend that you fork the
project and make it your own. There's some chance I'll accept major-change PRs,
but I want to keep the core code tight and maintainable more than I want it to
have features that don't immediately relate to the project I'm working on.


## Code spelunking guide
Things under active construction:

+ [`JITIR`][jitir]: faster compilation and better superscalar codegen
+ [`AMD64RE`][amdre]: on-the-fly processor profiling
+ [`AMD64JIT`][amdjit]: AMD64 JIT backend

Code that's going away or will be unrecognizably refactored:

+ [`Cursor`](src/Wumber/Cursor.hs)
+ [`Element`](src/Wumber/Element.hs)
+ [`GeometricConstraints`](src/Wumber/GeometricConstraints.hs)

Some places to start:

+ [`Wumber`](src/Wumber.hs): backend library
  + [`Wumber.Model`](src/Wumber/Model.hs): all supported modeling domains
  + [`Wumber.SymMath`](src/Wumber/SymMath.hs): symbolic math expressions
  + [`Wumber.SymJIT`][jit]: JIT compilation for symbolic expressions
  + [`Wumber.Constraint`][const]: constraint specification DSL
  + [`Wumber.EquationSolve`][esolv]: algebraic + numerical solver
+ [`WumberShell`](shell/WumberShell.hs): display and interactive reloading
  + [`Wumber.Element`](src/Wumber/Element.hs): graphics we render (yes, they're
    in the wrong namespace)

[ic]: http://implicitcad.org
[ss]: http://solvespace.com/index.pl
[fr]: https://en.wikipedia.org/wiki/Function_representation
[dc]: https://www.boristhebrave.com/2018/04/15/dual-contouring-tutorial/
[dog]: https://en.wikipedia.org/wiki/Eating_your_own_dog_food
[hint]: https://hackage.haskell.org/package/hint
[oscad]: https://en.wikipedia.org/wiki/OpenSCAD

[sym]: src/Wumber/SymMath.hs
[der]: src/Wumber/SymDerivative.hs
[jit]: src/Wumber/SymJIT.hs
[disk]: src/WumberShell/ComputedCache.hs
[jitir]: src/Wumber/JITIR.hs
[amdre]: src/Wumber/AMD64RE.hs
[amdjit]: src/Wumber/AMD64JIT.hs
[const]: src/Wumber/Constraint.hs
[esolv]: src/Wumber/EquationSolve.hs


## System libraries
On Ubuntu 18.04:

```sh
$ apt install libgsl-dev freeglut3-dev libblas-dev liblapack-dev
```

I'm going to try to reduce system dependencies as we go, although we'll probably
always depend on some GL backend for interactive display.
