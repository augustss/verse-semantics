# Copyright Epic Games, Inc. All Rights Reserved.
any := function (exists X). X

Tail := function (exists Xs):
  all:
    I:any
    I > 0
    Xs(I)

Tail(all{1})
