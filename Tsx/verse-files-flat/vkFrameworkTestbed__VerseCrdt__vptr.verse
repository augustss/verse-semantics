# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/Verse }

##########################################################
# vptr
##########################################################

vptr<public> := class<final>:
    UUID<public>:int = 0
    ReplicaId<public>:int = 0

    ## Compares two vptr instances for ordering. First by
    ## comparing UUID, then by ReplicaId if UUIDs are equal.
    ##
    ## Result: 
    ##    this < Other => -1
    ##    this = Other => 0 
    ##    this > Other => 1
    CompareTo<public>(Other:vptr)<computes>:int =
        if (UUID < Other.UUID) { return -1 }
        if (UUID > Other.UUID) { return 1 }

        if (ReplicaId < Other.ReplicaId) { return -1 }
        if (ReplicaId > Other.ReplicaId) { return 1 }

        # Equal
        return 0

MakeVPtr<public><constructor>(UUID:int, ReplicaId:int)<transacts> := vptr:
    UUID := UUID
    ReplicaId := ReplicaId

ToString<public>(VPtr:vptr)<transacts>:string =
    return "UUID: {VPtr.UUID} | ReplicaId: {VPtr.ReplicaId}"

##########################################################
# operation
##########################################################

operation<public> := class<abstract><internal><unique> {}

inserted_operation<public> := class<final><unique>(operation):
    After<public>:vptr
    At<public>:vptr
    Value<public>:crdt_value_interface

MakeInsertedOperation<public><constructor>(After:vptr, At:vptr, Value:crdt_value_interface)<transacts> := inserted_operation:
    After := After
    At := At
    Value := Value

ToString<public>(Op:inserted_operation)<transacts>:string =
    return "After: ({Op.After}), At: ({Op.At}), Value: ({Op.Value.Stringify()})"

removed_operation<public> := class<final><unique>(operation):
    At<public>:vptr

MakeRemovedOperation<public><constructor>(At:vptr)<transacts> := removed_operation:
    At := At

ToString<public>(Op:removed_operation)<transacts>:string =
    return "At: {Op.At}"