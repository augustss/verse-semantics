# Copyright Epic Games, Inc. All Rights Reserved.
var X:int = 0
exists Y
all. (set X = X + 1; Y = 1)|(set X = X + 2)
set X = 0
Y = 1
X
