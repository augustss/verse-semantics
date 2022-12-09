Tail := (lambda X . all{exists I . I > 0; X(I)})
Cons := (lambda X . exists Y . exists Z . (Y, Z) = X; all{Y | :Z})
MapAp := (lambda F . if (G := F(0)) {Cons(G(), MapAp(Tail(F)))})
MapAp(all{
  X := 1|2|3
  lambda Y . 4|X})
