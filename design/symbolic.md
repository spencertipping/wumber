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

`Sym` won't own any control flow beyond `ifzpos(x, a, b)`, a derivative-closed
way to calculate max and min.

`Sym` should be a `Functor` and work with `Storable` from a JIT perspective.
Then `jacobian` can give us a `V3 (Sym R)`, which we can turn into a
`V3 R -> V3 R`.
