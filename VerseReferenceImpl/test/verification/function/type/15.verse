# Copyright Epic Games, Inc. All Rights Reserved.
F(G(:t):t, X:t where t:type):t = X
H(3) := 4
H(4) := 5
H(5) := 5
Main() := F(H, 3) = 3|4|5
