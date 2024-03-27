# Copyright Epic Games, Inc. All Rights Reserved.

c0 := class:
  F0<private>(X:int):int = 40+X

c1 := class(c0):
  H1<public>(X:int):int = F0(X)

c1{}.H1(2)
