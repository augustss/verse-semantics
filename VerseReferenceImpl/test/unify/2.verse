# Copyright Epic Games, Inc. All Rights Reserved.
all:
  C := 2|3|4
  Y := all:
    B := 5|6|7
    X := one:
      exists A
      (A = 8; B = 6; A)|(A = 9; C = 3; A)
    X
  Y
