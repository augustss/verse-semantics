# Copyright Epic Games, Inc. All Rights Reserved.
FoldRight(F(:t, :u):u, (X:t, Y:t), Z:u where t:type, u:type):u = F(X, F(Y, Z))
G(:int, 3) := 4
G(:int, 4) := 5
G(:int, 5) := 5
Main() := FoldRight(G, (1, 2), 3) = 3|4|5
