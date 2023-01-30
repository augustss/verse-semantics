# Copyright Epic Games, Inc. All Rights Reserved.
var X = 0
Y :=
  all:
    all:
      set X = X + 1
      fail
    X
(X, Y)
