# Copyright Epic Games, Inc. All Rights Reserved.
exists X
var Y:int = 0
Z := all:
  (
    set Y = Y + 1
    X = 1|2
    Y
  ) | (
    set Y = Y + 2
    X = 2|3
    Y
  )
X = 1|2|3
Z
