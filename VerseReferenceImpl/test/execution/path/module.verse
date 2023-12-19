# Copyright Epic Games, Inc. All Rights Reserved.

Value := 0

Module1 := module:
  Value := 10

  Module2 := module:
    Value := 210

(/root:)Value, (/root/Module1:)Value, (/root/Module1/Module2:)Value
