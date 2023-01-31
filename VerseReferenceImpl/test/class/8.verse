# Copyright Epic Games, Inc. All Rights Reserved.
any(exists X) := X

Plus1(X:any) := X + 1

x := class:
  X:Plus1 = 1

X := x:
  X := 2

X.X
