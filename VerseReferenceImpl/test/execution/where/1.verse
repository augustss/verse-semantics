# Copyright Epic Games, Inc. All Rights Reserved.
any(exists X) := X

Type := any

tuple(ts:[]Type)(Xs:any) := for:
  I:int
  t := ts[I]
  X := Xs[I]
do:
  t[X]

operator'->'(t:Type, u:Type)(F:any)(X:t):u = F(X)

prefix'[]'(t:Type)(Xs:any) := for (X:Xs). t[X]

Curry(F:tuple(any, any) -> any)(X:any)(Y:any) := F(X, Y)

F(X:Curry(operator'+')(Y) where Y:int = 1) := X

F(2)
