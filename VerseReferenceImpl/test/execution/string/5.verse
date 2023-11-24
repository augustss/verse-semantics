# Copyright Epic Games, Inc. All Rights Reserved.
# Check that string is array of UTF8 bytes, and thta the pretty printer treats incorrect codes nicely
Xs := "aåb"
for(I->X:Xs). for (J->Y:Xs; J <> I). Y
