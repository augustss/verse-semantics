# Copyright Epic Games, Inc. All Rights Reserved.
any(exists X) := X

x := class:
  F()() := 1

y := class(x):
  F()() := 2

Y := y{}

Y.F()()
