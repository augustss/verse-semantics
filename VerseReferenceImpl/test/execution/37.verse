# Copyright Epic Games, Inc. All Rights Reserved.
exists logic
logic = (exists X => X = false|true)
exists struct1
struct1 = struct:
  Property:logic
X := struct1:
  Property := true
X.Property
