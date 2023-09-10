# Copyright Epic Games, Inc. All Rights Reserved.
any(exists X) := X

type := any

operator'->'(t:type, u:type)(F:any)(X:t):u = F(X)

exactly(X:int)(Y:any) := X = Y

F(X:int)(Y:int) := X + Y

G:int -> int -> exactly(0) = F

G(1)(-1)
