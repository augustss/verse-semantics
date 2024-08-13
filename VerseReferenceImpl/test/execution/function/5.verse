# Copyright Epic Games, Inc. All Rights Reserved.
Xtop:?Plus1 = option{1}

prefix'?'(t:Type)(X:xany) := if (Y := X?) then truth{t[Y]}

Plus1 := Curry(operator'+')(1)

Type := xany

Curry(F:tuple(xany, xany) -> xany)(X:xany)(Y:xany) := F[X, Y]

xany(exists X) := X

operator'->'(t:Type, u:Type)(F:xany)(X:t):u = F[X]

tuple(ts:[]Type)(Xs:xany) := for:
  I:int
  t := ts[I]
  X := Xs[I]
do t[X]

prefix'[]'(t:Type)(Xs:xany) := for (X:Xs). t[X]

Zip(Xs:[]xany, Ys:[]xany):[]tuple(xany, xany) = all:
  I:int
  (Xs[I], Ys[I])

Zip((1, 2, 3), (4, 5, 6))
