# Copyright Epic Games, Inc. All Rights Reserved.
var A:int = 0
for:
  for ((set A = A + 1000) | (set A = A + 10000)):
    (set A = A + 1) | (set A = A + 10) | (set A = A + 100)
