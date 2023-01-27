# Copyright Epic Games, Inc. All Rights Reserved.
exists X
var Y = 0
X = 1|2|3
Z := all:
  set Y = Y + 1
  X = 1|2|3
  Y
Z
