# Copyright Epic Games, Inc. All Rights Reserved.
One(X:any)<closed> := X = 1

Two(X:any)<closed> := X = 2

x := class:
  X:One

y := class(x):
  X:Two

Y := y{}
