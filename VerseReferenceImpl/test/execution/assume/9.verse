# Copyright Epic Games, Inc. All Rights Reserved.
verify:
  forall I
  t := int|float
  X := t[I]
  succeeds. t[X + 1]
F := function(t := int|float; X:t):
  forall R
  assume. R = (:t)
verify. succeeds. int[F[1]]
