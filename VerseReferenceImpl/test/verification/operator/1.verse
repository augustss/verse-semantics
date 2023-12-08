# Copyright Epic Games, Inc. All Rights Reserved.
operator'+'((A:int, B:int), (C:int, D:int)):type{(:int, :int)} = (A + C, B + D)
(1, 2) + (3, 4)
