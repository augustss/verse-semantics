exists false;
false = ();

exists ToRevList;
ToRevList = fun(Arg) {
  exists Result;
  Result = Alloc[];
  all { exists I; Write[Result, (Arg[I], Read[Result])] };
  Read[Result]
};

exists RevImpl;
RevImpl = fun(Arg) {
  exists Xs; exists Ys;
  Arg = (Xs, Ys);
  if (Xs = false) then(Xs) {
    Ys
  } else {
    exists X; exists Zs;
    Xs = (X, Zs);
    RevImpl[Zs, (X, Ys)]
  }
};

exists Rev;
Rev = fun(Arg) {
  RevImpl[Arg, false]
};

exists ToList;
ToList = fun(Arg) {
  Rev[ToRevList[Arg]]
};

ToList[1, 2, 3]
