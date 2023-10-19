# Copyright Epic Games, Inc. All Rights Reserved.
Y := all:
  var Z:int = 1
  (set Z = 2; X = Z + 1)|(X = Z + 2)
X := 2|3
Y
