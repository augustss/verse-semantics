# Copyright Epic Games, Inc. All Rights Reserved.
exists F
exists X
F = function (exists Y). X = Y;
X = false;
F(true)
