# Copyright Epic Games, Inc. All Rights Reserved.

s := struct:
  X<private>:int = 42
  H<public>:int = X

s{}.H
