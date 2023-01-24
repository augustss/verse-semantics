# Copyright Epic Games, Inc. All Rights Reserved.
any(exists X) := X
x := struct:
  X:any
X := x:
  X := 1
x[X]
X.X
