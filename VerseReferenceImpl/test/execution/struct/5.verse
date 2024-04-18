# Copyright Epic Games, Inc. All Rights Reserved.
x := struct:
  F():int = 1
X := x{}
Y:x = X
Y.F()
