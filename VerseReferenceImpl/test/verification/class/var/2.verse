# Copyright Epic Games, Inc. All Rights Reserved.
void(:any) := false

class1 := class:
  var Data1:int = 13
  Method1(Arg1:int):void =
    set Data1 = Arg1

X := class1{}

X.Method1(42)

X.Data1
