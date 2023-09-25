# Copyright Epic Games, Inc. All Rights Reserved.
any(exists X) := X

Fold(F:any, var Z:any, Xs:any) :=
  all. set Z = F(:Xs, Z)
  Z

Fold(operator'+', 0, (1, 2, 3))
