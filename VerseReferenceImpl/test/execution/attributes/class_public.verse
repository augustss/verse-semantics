# Copyright Epic Games, Inc. All Rights Reserved.

c0 := class:
  F0<public>(X:int):int = 40+X

c1 := class(c0):
  F1<public>(X:int):int = 20+X

C0 := c0{}
C1 := c1{}

c0{}.F0(1), c1{}.F0(2), c1{}.F1(4)
