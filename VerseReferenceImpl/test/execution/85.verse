# Copyright Epic Games, Inc. All Rights Reserved.
any(exists X) := X
Fact(X > 0; X:int) := X * Fact(X - 1)
Fact(X <= 0; X:int) := 1
Fact(3)
