# Copyright Epic Games, Inc. All Rights Reserved.
B := 1
all:
  A := 1|2
  all. (A = 1)|(B = 1)
  A
