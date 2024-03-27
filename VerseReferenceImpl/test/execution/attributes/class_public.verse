# Copyright Epic Games, Inc. All Rights Reserved.

c0 := class:
  var X<public>:int = 42
  F0<public>(X:int):int = 40+X

c1 := class(c0):
  F1<public>(X:int):int = 20+X

C0 := c0{}
C1 := c1{}

C0.X, C0.F0(1), C1.X, C1.F0(2), C1.F1(4)
