# Copyright Epic Games, Inc. All Rights Reserved.

m0 := module:
  F0<internal>(X:int):int = 40 + X

  m1<public> := module:
    F1<internal>(X:int):int = 20 + X

    c1<public> := class:
       M1<public>(X:int) := (F0(X), F1(X))


m0.m1.c1{}.M1(2)
