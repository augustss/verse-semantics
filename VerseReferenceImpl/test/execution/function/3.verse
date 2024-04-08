# Copyright Epic Games, Inc. All Rights Reserved.
any(exists X) := X

Type := any

prefix'[]'(t:Type)(Xs:any) :=
  for (X:Xs). t[X]

Zip(Xs:[]any, Ys:[]any) := all:
  I:int
  (Xs[I], Ys[I])

Zip((1, 2, 3), (4, 5, 6))
