# Copyright Epic Games, Inc. All Rights Reserved.
X := all:
  T := A|B
  (T = 1; A = 1; T)|(T = 2; B = 2; T)
A := 1|2
B := 2|3
X
