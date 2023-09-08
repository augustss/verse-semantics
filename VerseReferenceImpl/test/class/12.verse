# Copyright Epic Games, Inc. All Rights Reserved.
class1 := class:
  F():int = 0

class2 := class(class1):
  F():float = 0.0

class2{}.F()
