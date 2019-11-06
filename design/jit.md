# JIT-compilable numbers
The DSL/construction aspect of this is mostly trivial: overload `Num`,
`Fractional`, etc to provide primitive float ops. That gives us expression-level
support, minus conditionals.

Inputs can be from `Storable` structure; we get that from `%rdi`. Constants can
be from another such structure from `%rsi`. I'm not sure we can easily call back
into Haskell from a JIT context, nor am I sure we need to. The main purpose of
JIT is to flatten out the overhead from matrix/vector math and `hint`.

I think we want control flow: `if` and `while`. The assembler is a lot simpler
if every basic block is unaware of labels. We can just use a global offset
table, which amounts to indirect jumps. Every BB is assembled independently.


## Register allocation
...is simple enough, especially if we know spill costs and the cache size.
Numbers are immutable, so we have a lot of latitude to evaluate stuff in any
order. I think we could also search for mirrored operand chains and bundle
things into vector instructions.

If we do vector stuff, then we don't have register allocation in the usual
sense. It's more like "how can we sidecar things".

`addsd` preserves high-half contents, btw, so we can have some chain divergence
and potentially stash things into high-XMM positions.
