# Copyright Epic Games, Inc. All Rights Reserved.
F(G(H(:t):t):t, X:t where t:type):t =
  H(:any) := X
  G(H)

Main() :=
  G(H(1):type{1}) := 1
  F(G, 1) = 1
