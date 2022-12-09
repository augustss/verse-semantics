Tail := (lambda X . all{exists I . I > 0; X(I)})
Cons := (lambda X . exists Y . exists Z . (Y, Z) = X; all{Y | :Z})
MapAp := (lambda F . if (G := F(0)) {Cons(G(), MapAp(Tail(F)))} else {false})
F := (lambda X . 1|2)
Gs := all:
  F()
  X := 3|4
  F()
  lambda Y . X
MapAp(Gs)
