# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/Verse }

## <-- HACK -->
## Mutable parametric types are not supported in Verse yet.
## 
## For the time being values passed into the CRDT collections
## use this interface so we can debug the collections.
crdt_value_interface<public> := interface:
    Stringify()<transacts>:string

# <-- HACK -->
# CRDT value for integers. We need this since parametric types don't support
# ToString which we need for debugging unit tests.
crdt_int := class(crdt_value_interface):
    V<public>:int
    Stringify<override>()<transacts>:string = ToString(V)

MakeCrdtInt<public><constructor>(Value:int)<transacts> := crdt_int:
    V := Value