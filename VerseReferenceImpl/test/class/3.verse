any(exists X) := X

x := class:
  X:any

y := class(x):
  F() := X

Y := y{X := 1}

Y.F()
