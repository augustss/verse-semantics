# Copyright Epic Games, Inc. All Rights Reserved.
block:
  exists X; exists A; exists B
  X = () => false
  A = all. X
  B = all. X
  A = B
false
