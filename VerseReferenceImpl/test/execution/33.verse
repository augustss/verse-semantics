# Copyright Epic Games, Inc. All Rights Reserved.
exists X
exists Y
exists Z
Y = one{(X = false; true)|(Z = true; false)|true};
X = true;
Z = false;
Y
