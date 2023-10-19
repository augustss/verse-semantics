# Copyright Epic Games, Inc. All Rights Reserved.
any(exists X) := X

one_(X:any) := X = 1

two(X:any) := X = 2

x := class:
  X:one_

y := class(x):
  X:two

Y := y{}
