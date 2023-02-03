# Copyright Epic Games, Inc. All Rights Reserved.
Y := all:
  X := all:
    exists A
    (A = 8; B = 6; A)|(A = 9; C = 3; A)
  B := 5|6
  X
C := 3
Y
