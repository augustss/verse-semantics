# Copyright Epic Games, Inc. All Rights Reserved.
any(exists X) := X

x := class:
  X:any
  F() := X

y := class(x):
  X := 1

Y := y{}

Y.F()
