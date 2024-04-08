# Copyright Epic Games, Inc. All Rights Reserved.
X := all:
  Y:int
  F[Y]
  Z:int
  G[Z]
  (Y, Z)

F(Y := 1|2)<closed> := {}

G(Y := 3|4)<closed> := {}

X
