# Copyright Epic Games, Inc. All Rights Reserved.
any(exists X) := X

F(:any > 0) := 1

M := module:
  F(0) := 2

F(0)
