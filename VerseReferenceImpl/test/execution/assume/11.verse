# Copyright Epic Games, Inc. All Rights Reserved.
exists X
verify:
  succeeds:
    X = 2
    assume{X = 1}
X = 1
