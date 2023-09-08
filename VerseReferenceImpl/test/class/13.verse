# Copyright Epic Games, Inc. All Rights Reserved.
any(exists X) := X

class1 := class:
  F():any = 0

class2 := class(class1):
  F():float = 0.0

X := class2:
  F():int = 0

X.F()
