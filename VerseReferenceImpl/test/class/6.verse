# Copyright Epic Games, Inc. All Rights Reserved.
any(exists X) := X

x := class:
  F():int = 1

y := class(x):
  F():int = 2

Y := y:
  F():int = 3

Y.F()
