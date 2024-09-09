# Copyright Epic Games, Inc. All Rights Reserved.
Tail := function (exists X). all{Y := X[I]; exists I > 0; Y}
Cons := function (exists Y, exists Z) {all. Y | (:Z)}
MapAp := function (exists F). if (G := F[0]). Cons[G[], MapAp[Tail[F]]]
MapAp[all:
  X := 1|2|3
  function (exists Y). 4|X
]
