# Copyright Epic Games, Inc. All Rights Reserved.

c0 := class:
  F0<private>(X:int):int = 40 + X

  c1 := class:
    F1<private>(X:int):int = 20 + X
    H1<public>(X:int) := (F0(X), F1(X))


c0{}.c1{}.H1(2)
