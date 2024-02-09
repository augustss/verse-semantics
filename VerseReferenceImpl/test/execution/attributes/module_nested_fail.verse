# Copyright Epic Games, Inc. All Rights Reserved.

m0 := module:
  F0<internal>(X:int):int = 40 + X

  m1 := module:
    F1<internal>(X:int):int = 20 + X

  c1 := class:
     M1<public>(X:int) := (F0(X), m1.F1(X))


m0.c1{}.M1(2)
