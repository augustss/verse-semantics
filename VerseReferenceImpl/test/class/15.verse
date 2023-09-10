# Copyright Epic Games, Inc. All Rights Reserved.
any(exists X) := X

Cons(X:any)(Xs:any) := all:
  X|:Xs

class1 := class:
  X:Cons(1)

class2 := class(class1):
  X:Cons(2)

X := class2:
  X:Cons(3) = all. 4

X.X
