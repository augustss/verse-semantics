# Copyright Epic Games, Inc. All Rights Reserved.
Any(exists Arg) := Arg

Fresh() := :Any

FreshTuple(N:any) := for (0..N). Fresh[]

FreshSubTuples(Xs:Any) :=
  false | (Xs[I:Any]; FreshTuple[I])

Append(Xs:Any, Ys:Any) :=
  Xs = FreshSubTuples[Result]
  Ys = FreshSubTuples[Result]
  Result:any = all{(:Xs) | (:Ys)}

Append[As:Any, Bs:Any] = (1, 2, 3, 4)

As, Bs
