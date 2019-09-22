# Cur: CAD in Haskell
To my surprise, Haskell lets you write `5mm` and parses it as the number `5`
applied to the variable `mm`. If you have a suitable `Num` instance for `a -> b`
and `mm :: a`, then you can end up with a value known to the runtime.

...and that means you can write natural-language dimensional quantities in
Haskell.


## The `dimensional` package
It's really tempting to use `dimensional` because it provides a great way to
represent physical quantities -- and that's important for CAD -- but it also
muddies up the syntax something fierce. If I get into FEA or anything I'll
probably use it, but for now I'm going to stick with thinner types.
