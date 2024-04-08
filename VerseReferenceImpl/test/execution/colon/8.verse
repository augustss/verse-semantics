# Copyright Epic Games, Inc. All Rights Reserved.
Curry(F:tuple(any, any) -> any)(X:any)(Y:any) := F(X, Y)

tuple(ts:[]Type)(Xs:any) := for:
  I:int
  t := ts[I]
  X := Xs[I]
do:
  t[X]

prefix'[]'(t:Type)(Xs:any) := for (X:Xs). t[X]

operator'->'(t:Type, u:Type)(F:any)(X:t):u = F(X)

any(exists X) := X

Type := any

F(X->Y:Curry(operator'+')(1)) := (X, Y)

F(2)
