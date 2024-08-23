# Copyright Epic Games, Inc. All Rights Reserved.
xany(exists X) := X

Plus1(X:xany) := X + 1

F(X:Plus1) := X

F(1)
