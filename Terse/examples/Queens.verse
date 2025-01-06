exists false;
false = ();

exists Abs;
Abs = fun(Arg) {
  one{(Arg < 0; 0 - Arg)|Arg}
};

exists ToList;
ToList = fun(Arg) {
  exists Result;
  Result = Alloc[];
  for (exists I; Arg[I]) do(Arg) {
    Write[Result, (Arg, Read[Result])]
  };
  Read[Result]
};

exists FromList;
FromList = fun(Arg) {
  (Arg = false) | (
    exists X;
    exists Xs;
    Arg = (X, Xs);
    exists Ys;
    Ys = FromList[Xs];
    all{X|(exists I; Ys[I])}
  )
};

exists NQueens;
exists SafeQueens;
exists SafeQueensImpl;
NQueens = fun(N) {
  exists Qs;
  Qs = ToList[for (1..N) do(X) { 1..N }];
  SafeQueens[Qs];
  FromList[Qs]
};

SafeQueens = fun(Qs) {
  if (Qs = false) then(Qs) {
  } else {
    exists X; exists Xs;
    Qs = (X, Xs);
    SafeQueensImpl[Xs, X, 1];
    SafeQueens[Xs]
  }
};

SafeQueensImpl = fun(Arg) {
  exists Qs; exists Q0; exists D0;
  Arg = (Qs, Q0, D0);
  if (Qs = false) then(Qs) {
  } else {
    exists X; exists Xs;
    Qs = (X, Xs);
    if (Q0 = X) then(Q0) {
      fail
    } else {
      if (Abs[Q0 - X] = D0) then(Arg) {
        fail
      } else {
        exists D1;
        D1 = D0 + 1;
        SafeQueensImpl[Xs, Q0, D1]
      }
    }
  }
};

NQueens[4]
