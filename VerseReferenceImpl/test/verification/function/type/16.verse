# Copyright Epic Games, Inc. All Rights Reserved.
F(G(:t):t, X:t where t:type):t = X
H(X:int) := X + 1
Main(X:int) := int[F(H, X)]
