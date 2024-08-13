# Copyright Epic Games, Inc. All Rights Reserved.
<#
any(exists X) := X

GE(Y:any)(X:any) := X >= Y

LT(Y:any)(X:any) := X < Y

X:GE(0):LT(10) = 5
#>
x:int=5
