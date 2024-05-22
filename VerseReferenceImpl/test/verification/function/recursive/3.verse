# Copyright Epic Games, Inc. All Rights Reserved.
Eq2(X:int, Y:int, Z:int):type{X = 2, Y = 2, Z = 2} =
  F(X)
  G(Y)
  H(Z)
  X, Y, Z

F(X:int) := assume{X = 2}

G(X:int) := assume{X = 2}

H(X:int) := assume{X = 2}

Main() :=
  X:int = 3
  Y:int = 4
  Z:int = 5
  Eq2(X, Y, Z)
  X = 2
  Y = 2
  Z = 2
