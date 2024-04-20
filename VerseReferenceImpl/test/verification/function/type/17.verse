# Copyright Epic Games, Inc. All Rights Reserved.
F(G(:t):t, X:t where t:type):t = X
H(X:int):int = X + 1
Main() := int[F(H, 1)]
