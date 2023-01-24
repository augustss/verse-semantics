# Copyright Epic Games, Inc. All Rights Reserved.
X1 := # test
  1

X2 := <# test #>
  2

X3 := block: # test
  3

X4 := block: <# test #>
  4

X5 := () => #test
  5

X6 := () => <# test #>
  6

(X1, X2, X3, X4, X5(), X6())
