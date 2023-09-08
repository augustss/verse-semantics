# Copyright Epic Games, Inc. All Rights Reserved.
any(exists X) := X

class1 := class:
  F():int = 0

class2 := class(class1):
  G() := F()
  F():float = 0.0

class2{}.G()
