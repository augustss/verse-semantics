# Copyright Epic Games, Inc. All Rights Reserved.
# Check that string is array of UTF8 bytes, a that any future pretty printing of strings works also for non-utf8 strings.
Xs := "aåb"
for(I->X:Xs). for (J->Y:Xs; J <> I). Y
