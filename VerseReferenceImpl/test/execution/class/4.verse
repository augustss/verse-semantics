# Copyright Epic Games, Inc. All Rights Reserved.
Any(exists X) := X

One(X:Any) := X = 1

Two(X:Any) := X = 2

x := class:
  X:One

y := class(x):
  X:Two

Y := y{}
