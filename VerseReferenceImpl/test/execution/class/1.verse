# Copyright Epic Games, Inc. All Rights Reserved.
any(exists X) := X

x := class:
  X:any

y := class(x):
  Y:any

Y := y:
  X := 1
  Y := 2

Y.X, Y.Y
