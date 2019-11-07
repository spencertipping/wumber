# Construction DSL
A library to make isofunctions from more structured things. For example, how
would we make a hex-cap machine bolt? Common parts should be easy to describe.

This probably comes down to a mixture of high-level and low-level stuff. A
hex-cap bolt is one rotationally-symmetric part clipped into the hex head
profile (intersected with six planes). If we have an axis model we can describe
this in just a few operations.

Part of the challenge is producing isofunctions that aren't too expensive to
evaluate. For example, if we have a screw profile with 100 threads we shouldn't
apply the profile function 100 times per input point.
