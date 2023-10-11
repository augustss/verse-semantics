# Copyright Epic Games, Inc. All Rights Reserved.
x := class:
  F()():int = 1

y := class(x):
  F()():int = 2

Y := y{}

Y.F()()
