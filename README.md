# Wumber: CAD in Haskell
[![build status](https://travis-ci.org/spencertipping/wumber.svg?branch=master)](https://travis-ci.org/spencertipping/wumber)

**Work in progress: don't trust CI output**

Wumber will be a programmatic CAD DSL for Haskell. The focus is on multidomain
modeling: you describe the thing you're designing as Haskell data structures,
then have your model implement Wumber typeclasses to project it into views, FEA
simulations, constraint spaces, etc.

Wumber takes a lot from [colah's ImplicitCAD][ic] and [SolveSpace][ss]: it uses
[F-Rep][fr] with [dual contouring][dc] to compute boundary meshes, and both
isofunctions and constraint sets are described with [symbolic math][sym] that
provides [automatic differentiation][der] and backs into [JIT-compiled
functions][jit] for numerical steps.

Wumber is designed to be fast enough for interactive use, even for large
designs. Expensive steps are [cached to disk][disk] for reuse, and Wumber
supports live code reloading with [hint][hint] so you can iterate without
running `stack build`. The JIT compiler currently produces vector code that runs
5-30x faster than compiled GHC (I assume because it avoids all memory allocation
and function calls).

+ [What I'm reading to build this.](reading.md)
+ [Here's what I'm working on.](frontier.md)


## Code spelunking guide
Things under active construction:

+ [`Symbolic`][sym0] -> [`SymMath`][sym]: bugfixes and much better performance
+ [`JITIR`][jitir]: faster compilation and better superscalar codegen
+ [`AMD64RE`][amdre]: on-the-fly processor profiling
+ [`AMD64JIT`][amdjit]: AMD64 JIT backend

Code that's going away or will be unrecognizably refactored:

+ [`ConstraintSimplify`](src/Wumber/ConstraintSimplify.hs)
+ [`Cursor`](src/Wumber/Cursor.hs)
+ [`Element`](src/Wumber/Element.hs)
+ [`GeometricConstraints`](src/Wumber/GeometricConstraints.hs)
+ [`Symbolic`](src/Wumber/Symbolic.hs)
+ [`SymbolicAlgebra`](src/Wumber/SymbolicAlgebra.hs)
+ [`SymbolicDerivative`](src/Wumber/SymbolicDerivative.hs)

Some places to start:

+ [`Wumber`](src/Wumber.hs): backend library
  + [`Wumber.Model`](src/Wumber/Model.hs): all supported modeling domains
  + [`Wumber.SymMath`](src/Wumber/SymMath.hs): symbolic math expressions
  + [`Wumber.SymbolicJIT`][jit]: JIT compilation for symbolic expressions
  + [`Wumber.Constraint`][const]: constraint specification DSL
  + [`Wumber.ConstraintSolver`][csolv]: algebraic + numerical solver
+ [`WumberShell`](src/WumberShell.hs): display and interactive reloading

[ic]: http://implicitcad.org
[ss]: http://solvespace.com/index.pl
[fr]: https://en.wikipedia.org/wiki/Function_representation
[dc]: https://www.boristhebrave.com/2018/04/15/dual-contouring-tutorial/
[hint]: https://hackage.haskell.org/package/hint

[sym]: src/Wumber/SymMath.hs
[sym0]: src/Wumber/Symbolic.hs
[der]: src/Wumber/SymbolicDerivative.hs
[jit]: src/Wumber/SymbolicJIT.hs
[disk]: src/WumberShell/ComputedCache.hs
[jitir]: src/Wumber/JITIR.hs
[amdre]: src/Wumber/AMD64RE.hs
[amdjit]: src/Wumber/AMD64JIT.hs
[const]: src/Wumber/Constraint.hs
[csolv]: src/Wumber/ConstraintSolver.hs


## System libraries
On Ubuntu 18.04:

```sh
$ apt install libgsl-dev freeglut3-dev libblas-dev liblapack-dev
```

I'm going to try to reduce system dependencies as we go, although we'll probably
always depend on some GL backend for interactive display.
