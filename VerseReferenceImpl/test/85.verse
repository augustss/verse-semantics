# Copyright Epic Games, Inc. All Rights Reserved.
any := function (exists X). X
exists Fact
Fact = X:any > 0 => X * Fact(X - 1)
Fact = X:any <= 0 => 1
Fact(3)
