# Copyright Epic Games, Inc. All Rights Reserved.
verify:
  forall I
  X := int[I]
  succeeds:
    int[X + 1]
