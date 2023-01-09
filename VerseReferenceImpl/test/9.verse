# Copyright Epic Games, Inc. All Rights Reserved.
exists X
exists Y
Y = all{(X = false; true)|(X = true; false)};
X = false|true;
Y
