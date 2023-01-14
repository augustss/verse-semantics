any(exists X) := X

type := any

list(t:type) := X:any => X = false|truth{:any}

cons(t:type) := struct:
  Head:t
  Tail:list(t)

logic(X:any) := X = false|true

X := cons(logic):
  Head := true
  Tail := false

not cons(cons(any))[X]

X.Head, X.Tail
