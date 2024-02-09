# Copyright Epic Games, Inc. All Rights Reserved.

m := module:
  F<internal>(X:int):int = 40 + X
  H<public>(X:int):int = F(X)

m.H(2)
