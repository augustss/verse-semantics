# Copyright Epic Games, Inc. All Rights Reserved.
verify. succeeds:
  F(X:int):int = if (X > 0) then X * F(X - 1) else 1
  int[F(1)]
