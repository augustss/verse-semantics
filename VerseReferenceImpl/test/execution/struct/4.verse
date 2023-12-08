# Copyright Epic Games, Inc. All Rights Reserved.
Any(exists X) := X

type := Any

list(t:type) := (X:Any => X = false|truth{:Any})

cons(t:type) := struct:
  Head:t
  Tail:list(t)

logic(X:Any) := X = false|true

X := cons(logic):
  Head := true
  Tail := false

cons(logic)[X]

not cons(cons(any))[X]

X.Head, X.Tail
