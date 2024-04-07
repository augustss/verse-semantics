# Copyright Epic Games, Inc. All Rights Reserved.
FoldLeft(F(:t, :u):t, X:t, (Y:u, Z:u) where t:type, u:type):t =
  F(F(X, Y), Z)
Main() :=
  FoldLeft((0|1|2, X := 1|2) => X, 0, (1, 2)) = 0|1|2
