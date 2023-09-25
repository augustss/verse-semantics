# Copyright Epic Games, Inc. All Rights Reserved.

exists X

E := enum {
  A,
  B,
  C
}

X = E.A

(X = E.A, all{X = E.B})
