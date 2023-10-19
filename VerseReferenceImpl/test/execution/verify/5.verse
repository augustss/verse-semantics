# Copyright Epic Games, Inc. All Rights Reserved.
verify:
  forall X
  X = 1|2
  succeeds:
    exists Y
    if (X = 1) { Y = 1 } else { Y = 2 }
    if (Y = 1) { X = 1 } else { X = 2 }
