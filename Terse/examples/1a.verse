exists false;
false = ();

exists true;
true = all{false};

exists Compose;
Compose = fun(Arg) {
  exists F;
  exists G;
  Arg = (F, G);
  fun(Arg) {
    F[G[Arg]]
  }
};

exists Split;
Split = fun(Arg) {
  all{exists I; I = 0|1; all{exists J; Arg[J]; Arg[J * 2 + I]}}
};

exists Merge;
Merge = fun(Arg) {
  exists Xs;
  exists Ys;
  Arg = (Xs, Ys);
  exists I; I = Alloc[0];
  exists J; J = Alloc[0];
  exists Intersect; Intersect = fun(Arg) {
    exists X; X = Xs[Read[I]];
    exists Y; Y = Ys[Read[J]];
    if (X < Y) then(X) {
      Write[I, Read[I] + 1];
      X|Intersect[]
    } else {
      Write[J, Read[J] + 1];
      Y|Intersect[]
    }
  };
  exists Left; Left = fun(Arg) {
    exists X; X = Xs[Read[I]];
    Write[I, Read[I] + 1];
    X|Left[]
  };
  exists Right; Right = fun(Arg) {
    exists Y; Y = Ys[Read[J]];
    Write[J, Read[J] + 1];
    Y|Right[]
  };
  all{Intersect[]|Left[]|Right[]}
};

exists MergeSort;
MergeSort = fun(Arg) {
  if (Arg = false) then(Arg) {
    false
  } else {
    if (exists X; Arg = all{X}) then(Arg) {
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
  all{exists I; Write[Result, Arg[I] + Read[Result]]};
  Read[Result]
};

exists Abs;
Abs = fun(Arg) {
  if (Arg < 0) then(Arg) { 0 - Arg } else { Arg }
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

exists ReadLines;
ReadLines = fun(Arg) {
  exists Loop;
  Loop = fun(Arg) {
    exists Line; Line = GetLine[];
    exists Result; Result = ReadLine[Line];
    Result|Loop[]
  };
  all{Loop[]}
};

exists Unzip;
Unzip = fun(Arg) {
  all{exists I; I = 0|1; all{exists J; Arg[J][I]}}
};

exists Xs;
exists Ys;
(Xs, Ys) = Unzip[ReadLines[]];

Sum[ZipWith[
  Compose[Abs, operator'-'],
  MergeSort[Xs],
  MergeSort[Ys]]]
