# Copyright Epic Games, Inc. All Rights Reserved.
any := function (exists X). X

Tail := function (exists Xs):
  all:
    I:any
    Xs(I)
    I > 0

Tail(all{1})
