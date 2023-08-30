# Copyright Epic Games, Inc. All Rights Reserved.
x := class:
  X:int = Y

y := class(x):
  Y:int = 1

Y := y{}

Y.X, Y.Y
