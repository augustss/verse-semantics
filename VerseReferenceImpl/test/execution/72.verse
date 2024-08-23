# Copyright Epic Games, Inc. All Rights Reserved.
Tail := function (exists X). all{Y := X[I]; exists I > 0; Y}
Cons := function (exists Y, exists Z) {all. Y | (:Z)}
MapAp := function (exists H). if (G := H[0]). Cons(G(), MapAp(Tail(H)))
F := function (exists X) {Y := one{function (exists Z). 1|2}; Y[]}
MapAp(all{F[]; X := 3|4; function (exists Y). X})
