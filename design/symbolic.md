# New `Symbolic` implementation
`Sym` has several glaring problems:

1.  It uses derived `Ord` a lot, which means we're _O(n)_ for comparisons
2.  Variable tracking is inconsistent, so rewrites are slow
3.  We have too many intermediate types: it should just be one union
4.  Polynomial normal form should be opt-in, not required (in general, we should
    prefer forms that use fewer operations to evaluate)
5.  `padd` has a terrible bug resulting in inaccurate math
6.  Symbolic differentiation should be batteries-included
7.  In general, we have too many pointers
8.  Right now, _data types_ drive associativity/commutativity -- but this should
    be a runtime property
9.  More things should be driven by an `Eval`-like interface
10. Pattern matching and working with variables are just really awkward
11. We need real conditionals
12. Iteration would be nice so we can define Julia sets and other iterated
    systems
13. We really need multiple variable output; indirecting through Haskell
    destroys performance for gradient vector evaluations


## ...so let's redesign it
First, let's cache a lot more identity-data onto each node, and let's have each
node be the same type of thing: a prefix-applied _n_-ary function with a varset,
stateset, inline profile, and flat vector of operands. Modifications happen by a
head-chosen function?

I think it's fine not to have an integer domain. Everything can remain `double`;
the goal here is to compute real-valued functions, not to do general-purpose
programming.

How do we handle `Amb`, if at all? Maybe we don't need to store expanded forms.
Not storing them means we don't have any space risk.

`SymFn1` etc are good abstractions. I think we can merge even though they have
different arities; those can become runtime failures.

`Sym` won't own any control flow beyond `ifnn(x, a, b)`, a derivative-closed way
to calculate max and min.


## Use cases
1. Constructing values, some of which refer to `var` quantities
2. Fast algebraic constraint simplification
3. Fast isofunction JIT
4. Fast pre-JIT algebraic simplification, e.g. constant folding
5. Symbolic differentiation
6. Fast rewriting, e.g. "turn `var 0` into `var 2 + 5` and simplify"
7. Fast linear subsystem simplification

(2), (4), (6), and (7) are all similar problems: we want to avoid full _O(n)_
scans and rewrite operations; statistically, most subnodes are likely not to
interact with our variables.


## FEA and differential equations
I want to have electrical simulation back into constraint variables, and ideally
yield differential equations for certain cases. It's unclear whether we should
go with things like [Bhaskara's sine approximation][bsine] or whether we should
use Taylor series and lean on polynomial algorithms.

(Bhaskara's approximation is a rational over polynomial terms, which should be
easy enough to work with.)

[bsine]: https://en.wikipedia.org/wiki/Bhaskara_I%27s_sine_approximation_formula


## Algebraic structure and normalization
Generally speaking, we'll want to be able to normalize any expression after
rewriting -- but only insofar as that normalization simplifies things related to
that rewrite.

For example, the rewrite `x -> y` in a form that doesn't mention `y` offers no
new opportunities for simplification. I think we can observe this from the
profile, which should be the same in each case.
