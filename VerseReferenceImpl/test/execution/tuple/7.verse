# Copyright Epic Games, Inc. All Rights Reserved.
Tail := function (exists Xs):
  all:
    I:any
    Xs[I]
    I > 0

Tail(all{1})
