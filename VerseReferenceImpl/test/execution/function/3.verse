# Copyright Epic Games, Inc. All Rights Reserved.
any(exists X) := X

type := any

prefix'[]'(t:type)(Xs:any) :=
  for (X:Xs). t[X]

Zip(Xs:[]any, Ys:[]any) := all:
  I:int
  (Xs[I], Ys[I])

Zip((1, 2, 3), (4, 5, 6))
