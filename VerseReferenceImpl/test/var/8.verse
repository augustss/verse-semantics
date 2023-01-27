# Copyright Epic Games, Inc. All Rights Reserved.
exists X
exists Z
var Y = 0
R := all:
  set Y = Y + 1
  (X = 2)|(Z = 2)
  Y
X = 1|2|3
Z = 1|2|3
R
