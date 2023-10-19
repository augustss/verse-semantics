# Copyright Epic Games, Inc. All Rights Reserved.
Curry(F:tuple(any, any) -> any)(X:any)(Y:any) := F(X, Y)

tuple(ts:[]type)(Xs:any) := for:
  I:int
  t := ts[I]
  X := Xs[I]
do:
  t[X]

prefix'[]'(t:type)(Xs:any) := for (X:Xs). t[X]

operator'->'(t:type, u:type)(F:any)(X:t):u = F(X)

any(exists X) := X

type := any

F(X->Y:Curry(operator'+')(1)) := (X, Y)

F(2)
