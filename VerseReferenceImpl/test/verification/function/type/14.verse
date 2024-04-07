# Copyright Epic Games, Inc. All Rights Reserved.
FoldRight(F(:t, :u):u, (X:t, Y:t), Z:u where t:type, u:type):u = F(X, F(Y, Z))
G(:int, X := 0|1|2)<closed> := if (X = 2) then X else X + 1
Main()<decides>:int = FoldRight(G, (1, 2), 0) = 0|1|2
