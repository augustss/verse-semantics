# Copyright Epic Games, Inc. All Rights Reserved.
exists X
exists Y
Y = all {
  exists Z
  ((Z = 3)|(Z = 2)|(Z = 1));
  ((Z = X)|(Z = X)|(Z = X))
};
X = 1|2|3;
Y
