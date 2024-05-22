# Copyright Epic Games, Inc. All Rights Reserved.
Eq2(X:int):type{X = 2} =
  if (X = 2) then X else Eq2(X)

Main() :=
  X:int = 3
  X = 2
  Eq2(X)
