# Copyright Epic Games, Inc. All Rights Reserved.
exists F
F = function (exists A). all. (A = true; true)|(A = false; F(true))
exists X
exists Y
Y = F(X);
X = false;
Y
