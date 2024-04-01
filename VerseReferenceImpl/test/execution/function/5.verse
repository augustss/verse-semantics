# Copyright Epic Games, Inc. All Rights Reserved.
X:?Plus1 = option{1}

prefix'?'(t:Type)(X:any) := if (Y := X?) then truth{t[Y]}

Plus1 := Curry(operator'+')(1)

Type := any

Curry(F:tuple(any, any) -> any)(X:any)(Y:any) := F[X, Y]

any(exists X) := X

operator'->'(t:Type, u:Type)(F:any)(X:t):u = F[X]

tuple(ts:[]Type)(Xs:any) := for:
  I:int
  t := ts[I]
  X := Xs[I]
do t[X]

prefix'[]'(t:Type)(Xs:any) := for (X:Xs). t[X]

Zip(Xs:[]any, Ys:[]any):[]tuple(any, any) = all:
  I:int
  (Xs[I], Ys[I])

Zip((1, 2, 3), (4, 5, 6))
