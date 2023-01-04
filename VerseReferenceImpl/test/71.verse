Tail := function (exists X). all{exists I > 0; X(I)}
Cons := function (exists Y, exists Z) {all. Y | :Z}
MapAp := function (exists F). if (G := F(0)) {Cons(G(), MapAp(Tail(F)))} else {false}
all:
  MapAp(all:
    X := 1|2|3
    function (exists Y). 4|X
  )
