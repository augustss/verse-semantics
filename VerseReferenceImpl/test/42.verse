# Copyright Epic Games, Inc. All Rights Reserved.
exists X
exists Y
Y = all {
  exists Z
  (Z = X; Z = 1)|(Z = X; Z = 2)|(Z = X; Z = 0)
};
X = 1|2|3;
Y
