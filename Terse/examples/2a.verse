exists SafeDec;
exists SafeInc;

SafeDec = fun(Arg) {
  exists First; exists FirstSuffix;
  Arg = (First, FirstSuffix);
  exists Result;
  Result = ReadInt[FirstSuffix];
  if (
    exists Second; exists SecondSuffix;
    Result = all{Second, SecondSuffix}; (Second, SecondSuffix)
  ) then(Arg) {
    exists Second; exists SecondSuffix;
    Arg = (Second, SecondSuffix);
    if (First < Second) then(Arg) {
      0
    } else {
      if (Second < First) then(Arg) {
        if (Second + 3 < First) then(Arg) {
          0
        } else {
          SafeDec[Second, SecondSuffix]
        }
      } else {
        0
      }
    }
  } else {
    1
  }
};

SafeInc = fun(Arg) {
  exists First; exists FirstSuffix;
  Arg = (First, FirstSuffix);
  exists Result;
  Result = ReadInt[FirstSuffix];
  if (
    exists Second; exists SecondSuffix;
    Result = all{Second, SecondSuffix}; (Second, SecondSuffix)
  ) then(Arg) {
    exists Second; exists SecondSuffix;
    Arg = (Second, SecondSuffix);
    if (First < Second) then(Arg) {
      if (First + 3 < Second) then(Arg) {
        0
      } else {
        SafeInc[Second, SecondSuffix]
      }
    } else {
      0
    }
  } else {
    1
  }
};

exists Safe;
Safe = fun(Arg) {
  exists First; exists FirstSuffix;
  Arg = (First, FirstSuffix);
  exists Result;
  Result = ReadInt[FirstSuffix];
  if (
    exists Second; exists SecondSuffix;
    Result = all{Second, SecondSuffix}; Second, SecondSuffix
  ) then(Arg) {
    exists Second; exists SecondSuffix;
    Arg = (Second, SecondSuffix);
    if (First < Second) then(Arg) {
      if (First + 3 < Second) then(Arg) {
        0
      } else {
        SafeInc[Second, SecondSuffix]
      }
    } else {
      if (Second < First) then(Arg) {
        if (Second + 3 < First) then(Arg) {
          0
        } else {
          SafeDec[Second, SecondSuffix]
        }
      } else {
        0
      }
    }
  } else {
    1
  }
};

exists SafeLine;
SafeLine = fun(Line) {
  exists Result;
  Result = ReadInt[Line];
  if (
    exists First; exists FirstSuffix;
    Result = all{First, FirstSuffix}; First, FirstSuffix
  ) then(Arg) {
    Safe[Arg]
  } else {
    fail
  }
};

exists SafeLinesImpl;
SafeLinesImpl = fun(X) {
  exists Line;
  Line = GetLine[];
  if (SafeLine[Line]) then(Y) {
    SafeLinesImpl[X + Y]
  } else {
    X
  }
};

exists SafeLines;
SafeLines = fun(Arg) {
  SafeLinesImpl[0]
};

SafeLines[]
