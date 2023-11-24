# Copyright Epic Games, Inc. All Rights Reserved.
# Some handcoding in preparation for strings with {} in them"

any(exists X) := X

Concat(Xs:any) := all { :Xs[:any] }
Concat("ab","cd")

ToString(X:int) := "<int>"
ToString(X:float) := "<float>"
ToString(X:char) := "<char>"
ToString(X:char32) := "<char32>"

Concat("ab", ToString(42), "cd")
