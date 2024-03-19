# Copyright Epic Games, Inc. All Rights Reserved.

m0 := module:
  F0<private>(X:int):int = 40 + X
  H0<public>(X:int):int = F0(X)
  m1<public> := module:
    H1<public>(X:int):int = F0(X)

m0.H0(2), m0.m1.H1(1)
