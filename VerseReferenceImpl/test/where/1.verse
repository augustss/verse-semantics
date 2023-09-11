# Copyright Epic Games, Inc. All Rights Reserved.
any(exists X) := X

type := any

tuple(ts:[]type)(Xs:any) := for:
  I:int
  t := ts[I]
  X := Xs[I]
do:
  t[X]

operator'->'(t:type, u:type)(F:any)(X:t):u = F(X)

prefix'[]'(t:type)(Xs:any) := for (X:Xs). t[X]

Curry(F:tuple(any, any) -> any)(X:any)(Y:any) := F(X, Y)

F(X:Curry(operator'+')(Y) where Y:int = 1) := X

F(2)
