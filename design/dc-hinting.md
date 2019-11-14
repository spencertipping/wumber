# Dual contour hinting
Dual contour nodes will miss features sharp enough to avoid cell corners. One
way around this is to provide a hint: "point `X` is on the surface" or "is of
interest", the implication being "you need to bisect until you get there".

Practically speaking there are some problems:

1. We often have infinite structures we CSG down to a reasonable size (how would
   we hint them?)
2. Manual hinting arguably defeats the whole purpose of F-rep

...actually, that's enough to convince me that we don't need it yet. I'm going
to work on getting the DC error rate down first.
