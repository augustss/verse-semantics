# Copyright Epic Games, Inc. All Rights Reserved.
any(exists X) := X

Type := any

operator'->'(t:Type, u:Type)(F:any)(X:any) := u[F[t[X]]]

Plus1(X:any) := X + 1

F:Plus1 -> Plus1 = (X:any => X)

F(1)
