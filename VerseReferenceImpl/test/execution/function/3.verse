# Copyright Epic Games, Inc. All Rights Reserved.
xany(exists X) := X

Type := xany

prefix'[]'(t:Type)(Xs:xany) :=
  for (X:Xs). t[X]

Zip(Xs:[]xany, Ys:[]xany) := all:
  I:int
  (Xs[I], Ys[I])

Zip((1, 2, 3), (4, 5, 6))
