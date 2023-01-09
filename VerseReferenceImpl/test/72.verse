# Copyright Epic Games, Inc. All Rights Reserved.
Tail := function (exists X). all{exists I > 0; X(I)}
Cons := function (exists Y, exists Z) {all. Y | :Z}
MapAp := function (exists F). if (G := F(0)). Cons(G(), MapAp(Tail(F)))
F := function (exists X) {Y := one{function (exists Z). 1|2}; Y()}
MapAp(all{F(); X := 3|4; function (exists Y). X})
