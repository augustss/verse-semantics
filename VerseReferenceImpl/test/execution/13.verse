# Copyright Epic Games, Inc. All Rights Reserved.
exists X
exists Z
Z = all {
  exists Y
  Y = X;
  Y = false
};
X = true;
Z
