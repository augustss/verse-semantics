# Copyright Epic Games, Inc. All Rights Reserved.
exists A
exists B
B = all {
  exists C
  exists D
  C = D
  A = C
  D = false
}
A = true
(A, B)
