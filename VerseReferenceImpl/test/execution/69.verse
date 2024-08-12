# Copyright Epic Games, Inc. All Rights Reserved.
Tail := function (exists X). all{Y := X[I]; exists I > 0; Y}
Cons := function (exists Y, exists Z) {all. Y | (:Z)}
MapAp := function (exists F). if (G := F[0]). Cons(G(), MapAp[Tail(F)])
H := function (). 1|2
MapAp(all:
  H[]
  X := 3|4
  H[]
  function (). X
)
