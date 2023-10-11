# Copyright Epic Games, Inc. All Rights Reserved.
verify:
  forall X
  X = 1|2
  succeeds:
    if (X = 1|2) { 1 } else { Err }
