# Copyright Epic Games, Inc. All Rights Reserved.
verify. succeeds:
  F(t := int|float; X:t):t = X + 1
  float[F(1)]
