# Copyright Epic Games, Inc. All Rights Reserved.

A := 10

Module1 := module:
  A<public> := 20

class1 := class:
   A:int := (/root:)A
   B:int := (/root/Module1:)A

Obj := class1{}

Obj.A, Obj.B
