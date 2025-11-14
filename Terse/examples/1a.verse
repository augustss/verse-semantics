exists false;
false = ();

exists Compose;
Compose = fun(Arg) {
  exists F;
  exists G;
  Arg = (F, G);
  fun(Arg) {
    F[G[Arg]]
  }
};

exists Length;
Length = fun(Arg) {
  if (Arg = false) then(Arg) {
    0
  } else {
    exists X; exists Xs;
    Arg = (X, Xs);
    Length[Xs] + 1
  }
};

exists FromList;
FromList = fun(Arg) {
  exists N;
  N = Length[Arg];
  exists Xs;
  Xs = Alloc[Arg];
  all{1..N; exists X; exists Ys; (X, Ys) = Read[Xs]; Write[Xs, Ys]; X}
};

exists ToRevList;
ToRevList = fun(Arg) {
  exists Xs;
  Xs = Alloc[];
  all {
    exists I;
    Write[Xs, (Arg[I], Read[Xs])]
  };
  Read[Xs]
};

exists RevImpl;
RevImpl = fun(Arg) {
  exists Xs;
  exists Ys;
  Arg = (Xs, Ys);
  (
    Xs = false;
    Ys
  ) | (
    exists Z;
    exists Zs;
    Xs = (Z, Zs);
    RevImpl[Zs, (Z, Ys)]
  )
};

exists Rev;
Rev = fun(Arg) {
  RevImpl[Arg, false]
};

exists ToList;
ToList = fun(Arg) {
  Rev[ToRevList[Arg]]
};

exists Split;
Split = fun(Arg) {
  one {
    (
      exists X;
      exists Xs;
      Arg = (X, Xs);
      one {
        (
          exists Y;
          exists Ys;
          Xs = (Y, Ys);
          one {
            (
              exists Left;
              exists Right;
              Split[Ys] = (Left, Right);
              (X, Left), (Y, Right)
            ) | (
              ((X, false), (Y, false))
            )
          }
        ) | (
          ((X, false), false)
        )
      }
    ) | false
  }
};

exists Merge;
Merge = fun(Arg) {
  exists X0;
  exists Y0;
  Arg = (X0, Y0);
  if (X0 = false) then(X0) {
    Y0
  } else {
    if (Y0 = false) then(Y0) {
      X0
    } else {
      exists X1;
      exists X2;
      X0 = (X1, X2);
      exists Y1;
      exists Y2;
      Y0 = (Y1, Y2);
      if (X1 < Y1) then(X1) {
        (X1, Merge[X2, Y0])
      } else {
        if (Y1 < X1) then(Y1) {
          (Y1, Merge[X0, Y2])
        } else {
          (X1, (Y1, Merge[X2, Y2]))
        }
      }
    }
  }
};

exists MergeSort;
MergeSort = fun(Arg) {
  if (Arg = false) then(Arg) {
    false
  } else {
    if (exists X; Arg = (X, false)) then(Arg) {
      Arg
    } else {
      exists Left;
      exists Right;
      Split[Arg] = (Left, Right);
      Merge[MergeSort[Left], MergeSort[Right]]
    }
  }
};

exists ZipWith;
ZipWith = fun(Arg) {
  exists F;
  exists Xs;
  exists Ys;
  (F, Xs, Ys) = Arg;
  for (exists I; Xs[I], Ys[I]) do(Arg) {
    F[Arg]
  }
};

exists Sum;
Sum = fun(Arg) {
  exists Result;
  Result = Alloc[0];
  all {
    exists I;
    Write[Result, Arg[I] + Read[Result]]
  };
  Read[Result]
};

exists Abs;
Abs = fun(Arg) {
  one {(Arg < 0; 0 - Arg) | Arg}
};

exists ReadLine;
ReadLine = fun(Arg) {
  exists First;
  exists FirstSuffix;
  all{First, FirstSuffix} = ReadInt[Arg];
  exists Second;
  exists SecondSuffix;
  all{Second, SecondSuffix} = ReadInt[FirstSuffix];
  (First, Second)
};

exists ReadLinesImpl;
ReadLinesImpl = fun(Arg) {
  exists Xs;
  exists Ys;
  (Xs, Ys) = Arg;
  exists Line;
  Line = GetLine[];
  if (ReadLine[Line]) then(Arg) {
    exists X;
    exists Y;
    (X, Y) = Arg;
    ReadLinesImpl[(X, Xs), (Y, Ys)]
  } else {
    Arg
  }
};

exists ReadLines;
ReadLines = fun(Arg) {
  ReadLinesImpl[false, false]
};

exists Xs;
exists Ys;
(Xs, Ys) = ReadLines[];

Sum[ZipWith[
  Compose[Abs, operator'-'],
  FromList[MergeSort[Xs]],
  FromList[MergeSort[Ys]]]]
