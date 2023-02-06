# Copyright Epic Games, Inc. All Rights Reserved.
C := 3
all:
  B := 5|6
  X := all:
    exists A
    (A = 8; B = 6; A)|(A = 9; C = 3; A)
  X
