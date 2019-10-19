# Constraints
[SolveSpace](http://solvespace.com) allows you to define models using
objects/shapes which are initially arbitrary and are then gradually constrained
to axes and to each other.

![image](http://images.libregraphicsworld.org/cad/2013/08/solvespace-gpled/sketch.png)

[Here's its list of constraint
types.](https://github.com/solvespace/solvespace/blob/master/src/constraint.cpp#L11)

Many of these constraints produce nonlinear equations, so I think it uses a
numerical solver to minimize error -- but I can't find that in the source so I'm
not 100% sure. [colah mentions that gradient descent works for
this.](https://news.ycombinator.com/item?id=9249305)


## Common problems with constraints (which we won't have)
From [this comment](https://news.ycombinator.com/item?id=9250546):

> As a frequent user of Solidworks for ~4 years, I'd have to guess that CAD
> constraint solvers are not an easy problem.
>
> It happens pretty often that you'll modify a single constraint (flipping it is
> a common culprit), the solver will get "confused" and flip a bunch of your
> other constraints around. Constraint conflict resolution also often gets very
> hairy to almost impossible. Sometimes the fastest thing to do is to nuke a
> good part of your constraints and start over -- in a saner way.
>
> I guess the BIG CAD packages (CATIA, NX) might be a bit better at this, or at
> least better at strongly encouraging saner ways of build parametric
> constraints (which is what I've heard).

Basically, it's the collaborative-editor problem with the computer as a
collaborator. You flip a constraint and it's trying to fix your thing by
"fixing" other constraints, only to break a bunch of stuff with no way for you
to gracefully back out.

We don't have problems like this because everything we do originates from
source; the computer can't modify your code. Worst case, it tells you your
constraints don't converge and shows you a model you don't want.

Colah replied to the comment above and described some design elements in his
follow-on constraint solving CAD system (which was never released):

> There's some really powerful tools from algebraic geometry I tried to apply to
> this. In particular, there's these things called Grobner Bases which you can
> use to test if certain polynomials imply that another is true, false, or if
> it's independent of them.
> 
> From the users perspective, I wanted to use these to do a few things.
> 
> (1) Display implied constraints. If you make three corners of a quadrilateral
> perpendicular, the fourth is forced to be perpendicular. I grayed out
> perpendicular constraint could alert you to this, and prevent you from adding
> a conflicting constraint. Obviously, that example is kind of silly, but I
> think that propagating constraints through the whole model would really help
> people understand how more complicated models are constrained.
> 
> (2) If you try to add a constraint which conflicts with preexisting ones,
> highlight in red a minimal set it conflicts with.
> 
> I never got this part of the project to work though. I did some very small
> proof of concepts in sage, but couldn't really get things to work. I'm not
> really sure how realistic it is to make these work for real models.

I couldn't make any sense of the wikipedia article, but from colah's comment it
sounds like Gröbner bases give you a way to relate constraints to each other (P
implies Q, P contradicts Q, etc) that's faster to infer than trying to
numerically solve every pair of them. The purpose here is to display useful
error messages and have the computer understand which things it needs to
autocorrect when you introduce a contradiction into the model.

Again, we don't have these problems because our models are never edited: every
time you run it from source is pure, so there's no state-transition for us.


## Specifying constraints in Haskell
Constraints are backreferences: you create a shape/line/whatever first, then
refer to it later to constrain it.

```haskell
-- the current approach: constraints baked in using haskell variables
axle ø l = extrude circle l
  where circle = spin (line (ø / 2)) 360

-- constrain-later approach: create objects that have identity, then refer back
-- and modify them
axle = extrude (spin line) l
my_axle ø l = axle do
  len "line"    =: ø / 2
  angle "spin"  =: 360
  len "extrude" =: l

-- a better constraint system using lenses (and we could probably add some
-- implicit inference to this to reduce redundancy)
axle = extrude (spin line) l
my_axle ø l = axle do
  _form._form._len =: ø / 2
  _len             =: l
  _form._angle     =: 360

-- even better: mix the two systems by propagating haskell values as usual, but
-- making those values be constraints rather than always constants
axle ø l = extrude (spin 360 $ line (ø / 2)) l

do shared_l <- var
   shared_ø <- var
   let a1 = axle shared_ø shared_l
       a2 = axle shared_ø shared_l
   shared_l =: shared_ø ** 2       -- example: tie length to diameter
   ...
```

**TODO**: what next?
