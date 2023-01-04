Tail := function (exists X). all{exists I > 0; X(I)}
Cons := function (exists Y, exists Z) {all. Y | :Z}
MapAp := function (exists F). if (G := F(0)). Cons(G(), MapAp(Tail(F)))
F := function (). 1|2
MapAp(all:
  F()
  X := 3|4
  F()
  function (). X
)
