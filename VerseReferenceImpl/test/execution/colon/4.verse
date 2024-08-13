# Copyright Epic Games, Inc. All Rights Reserved.
xany(exists X) := X

GE(Y:xany)(X:xany) := X >= Y

LT(Y:xany)(X:xany) := X < Y

:GE(0):LT(10) = 10
