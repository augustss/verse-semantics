# Copyright Epic Games, Inc. All Rights Reserved.
all:
  Y := all:
    X := one:
      exists A
      (A = 8; B = 6; A)|(A = 9; C = 3; A)
    B := 5|6|7
    X
  C := 2|3|4
  Y
