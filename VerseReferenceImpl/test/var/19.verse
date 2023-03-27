# Copyright Epic Games, Inc. All Rights Reserved.
Fold(exists F, var Z, exists Xs) :=
  all. set Z = F(:Xs, Z)
  Z

Fold(operator'+', 0, (1, 2, 3))
