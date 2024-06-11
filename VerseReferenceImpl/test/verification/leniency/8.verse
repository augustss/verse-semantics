# Copyright Epic Games, Inc. All Rights Reserved.
Y := all{X:type{3|4} = X}
if (Y[1]):
  Y(0) = 3|4
  Y(1) = 3|4
else if (Y[0]):
  Y(0) = 3|4
else:
  Y = ()
