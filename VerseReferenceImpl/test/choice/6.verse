# Copyright Epic Games, Inc. All Rights Reserved.
any(exists Arg) := Arg

Fresh() := :any

FreshTuple(N:any) := all{I := 0..N; Fresh()}

FreshSubTuples(Xs:any) :=
  false | (Xs(I:any); FreshTuple(I))

Append(Xs:any, Ys:any) :=
  Xs = FreshSubTuples(Result)
  Ys = FreshSubTuples(Result)
  Result:any = all{:Xs | :Ys}

Append(As:any, Bs:any) = (1, 2, 3, 4)

As, Bs
