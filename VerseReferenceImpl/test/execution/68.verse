# Copyright Epic Games, Inc. All Rights Reserved.
Tail :=
  exists X => all{Y := X[I]; exists I > 0; Y}
Cons := function (exists Y, exists Z). all{Y | (:Z)}
MapAp :=
  exists F => if (G := F[0]). Cons(G(), MapAp[Tail(F)])
F :=
  () => 1|2
MapAp[all{F[]; X := 3|4; () => X}]
