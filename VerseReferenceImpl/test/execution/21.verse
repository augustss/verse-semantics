# Copyright Epic Games, Inc. All Rights Reserved.
exists X
exists Y
Y = all{(exists Z = true; X = false; true) | (X = true; false)};
X = false;
X
