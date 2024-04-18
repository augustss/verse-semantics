# Copyright Epic Games, Inc. All Rights Reserved.
x(Arg:int) := struct:
  F():int = Arg
X := x(1){}
Y:x(2) = X
Y.F()
