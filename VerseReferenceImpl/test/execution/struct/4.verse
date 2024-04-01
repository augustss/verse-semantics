# Copyright Epic Games, Inc. All Rights Reserved.
Any(exists X) := X

Type := Any

list(t:Type) := (X:Any => X = false|truth{:Any})

cons(t:Type) := struct:
  Head:t
  Tail:list(t)

logic(X:Any) := X = false|true

X := cons(logic):
  Head := true
  Tail := false

cons(logic)[X]

not cons(cons(any))[X]

X.Head, X.Tail
