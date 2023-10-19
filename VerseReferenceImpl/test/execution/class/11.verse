# Copyright Epic Games, Inc. All Rights Reserved.
x := class:
  X:int = Y

y := class(x):
  Y:int = 1

Z := y{}

Z.X, Z.Y
