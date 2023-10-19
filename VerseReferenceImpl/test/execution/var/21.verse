# Copyright Epic Games, Inc. All Rights Reserved.
any(exists X) := X

Curry(F:any)(X:any)(Y:any) := F[X, Y]

var X:Curry(operator'+')(1) = 0
Y := X
set X = X
(X, Y)
