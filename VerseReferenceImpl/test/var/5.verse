# Copyright Epic Games, Inc. All Rights Reserved.
var X = 0
exists Y
for:
  set X = X + 1
  Y = 1
do:
  set X = X + 2
set X = X / 3
Y = 1
X
