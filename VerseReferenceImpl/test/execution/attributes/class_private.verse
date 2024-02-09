# Copyright Epic Games, Inc. All Rights Reserved.

c0 := class:
  F0<private>(X:int):int = 40+X
  H0<public>(X:int):int = F0(X)

c1 := class(c0):
  F1<private>(X:int):int = 20+X
  H1<public>(X:int):int = F1(X)

C0 := c0{}
C1 := c1{}

C0.H0(1), C1.H0(2), C1.H1(4)
