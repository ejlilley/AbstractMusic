AbstractMusic
=============

A general framework for constructing and manipulating different kinds
of notes (degrees of scales; intervals that are members of some
algebra; frequencies; etc.) and transforming between them (by applying
a concrete scale; applying some tuning system; etc.).

As an example, the data type `AbstractPitch2` in `Music.hs` is the
basic representation of musical pitch; it forms the points in an
affine space, with the standard musical interval (denoted
`AbstractInt2`) forming the associated vectors between points. The
underlying representation of `AbstractInt2` is as a rank-2 free
Abelian group (using a particular pair of intervals as a basis). Hence
the easiest way of tuning these pitches/intervals is with a rank-2
(aka syntonic, aka meantone) temperament, of which the notable
examples are Pythagorean and quarter-comma meantone tuning (see
`Tuning.hs`). Rank-1 tuning systems (equal temperament, e.g. 12-TET)
can also be used, by judicious application of the vector dot product
-- projecting the 2D vectors/points of `AbstractPitch2`'s vector space
onto some 1D line, which is then split up into 12/19/31/etc. equal
pieces (most commonly 12).

Please see my
[web page about Guillaume Costeley](http://www.ugnus.uk.eu.org/~edward/costeley/)
and 19-equal temperament for a quick demo of the part of this project
that deals with tuning systems.


