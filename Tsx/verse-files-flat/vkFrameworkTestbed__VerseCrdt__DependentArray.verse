# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/Verse }

##########################################################
# DependentArray
##########################################################
## DependentArray is a CRDT which aims to mimic the behavior of a regular
## Verse array type. 
##
## The key addition DependentArray brings is that one array may depend on the 
## values of another. Changes made to base arrays are automatically reflected
## in derived arrays, with ordering remaining consistent even as changes are
## made at different parts of the dependency hierarchy.
##
## Example:
##    A:dependent_array := MakeDependentArray(1)
##    B:dependent_array := A.GetDependentArray()
##    A.Append(1)
##    B.Append(2)
##    A.Append(3)
##    // RESULT: A = {1,3}, B = {1,2,3}
dependent_array<public> := class<final><unique>:
    ##########################################################
    # Members
    ##########################################################
    Id<internal>:int = 0
    var Rga<internal>:rga = rga{}

    var DepId<internal>:int = 0 # ID to use for the next dependent array
    var DependentArrays<internal>:[]dependent_array = array{}

    # BaseArray reference. This is needed so we can stream these in/out
    # while rebuilding the dependency chain.
    var BaseArray<internal>:?dependent_array = false

    ##########################################################
    # Recursive Broadcasts
    ##########################################################

    # Relay changes to dependent arrays. This may recurse through
    # many layers of dependencies.
    ApplyInsertOp<private>(Op:inserted_operation)<decides><transacts>:void =
        set Rga = Rga.ApplyInserted[Op]

        # Broadcast append to dependent arrays
        for (Arr : DependentArrays):
            Arr.ApplyInsertOp[Op]

    # Relay changes to dependent arrays. This may recurse through
    # many layers of dependencies.
    ApplyRemovedOp<private>(Op:removed_operation)<decides><transacts>:void =
        set Rga = Rga.ApplyRemoved[Op]

        # Broadcast append to dependent arrays
        for (Arr : DependentArrays):
            Arr.ApplyRemovedOp[Op]

    ##########################################################
    # Public API
    ##########################################################

    GetDependentArray<public>()<transacts>:dependent_array = 
        set DepId += 1
        Result := MakeDependentArray(DepId)
        Result.ConnectToBase(Self)
        return Result

    Append<public>(Value:crdt_value_interface)<decides><transacts>:void =
        Op := Rga.CreateAppend[Value]
        ApplyInsertOp[Op]

    Insert<public>(InsertionIndex:int, Element:crdt_value_interface)<decides><transacts>:void =
        Op := Rga.CreateInserted[InsertionIndex, Element]
        ApplyInsertOp[Op]

    Remove<public>(Index:int)<decides><transacts>:void =
        Op := Rga.CreateRemoved[Index]
        ApplyRemovedOp[Op]

    Length<public>()<transacts>:int =
        var Count:int = 0
        for (V : Rga.Vertices; not V.IsTombstone()?) { set Count += 1 }
        return Count

    Get<public>(Index:int)<decides><transacts>:crdt_value_interface =
        return Rga.GetArray()[Index]

    # Concatenate
    # Slice
    # RemoveElement
    # RemoveFirstElement
    # RemoveAllElements
    # ReplaceElement
    # ReplaceAllElements
    # ReplaceAll

    ## KSTODO: Evaluation of mutable vs. immutable for this type of collecition
    Clone<public>()<transacts>:dependent_array =
        NewArray := dependent_array:
            Id := Id
            DepId := DepId
            Rga := Rga

    ##########################################################
    # Internal/Private API
    ##########################################################

    GetArray<internal>()<transacts>:[]crdt_value_interface =
        return Rga.GetArray()

    ConnectToBase(Base:dependent_array)<transacts>:void =
        MergeDisconnectedChangesFromBaseArray(Base)
        set Base.DependentArrays += array{Self}
        set BaseArray = option{Base}

    DisconnectFromBase()<transacts>:void =
        if:
            Base := BaseArray?
            set Base.DependentArrays = Base.DependentArrays.RemoveFirstElement[Self]

        set BaseArray = false

    MergeDisconnectedChangesFromBaseArray<internal>(A:dependent_array)<transacts>:void =
        var Ops:[]operation = array{}
        for (I := 0..A.Rga.Vertices.Length; AV := A.Rga.Vertices[I]):
            BV := FindVertex(Rga.Vertices, AV.VPtr)

            if:
                not BV?
                Prev := A.Rga.Vertices[I-1]
                Op := MakeInsertedOperation(Prev.VPtr, AV.VPtr, AV.Value?)
                set Ops += array{Op}
            else if:
                # If there is a tombstone in the base, but not in the derived we need
                # to add a RemoveOp
                AV.IsTombstone()?
                not BV?.IsTombstone()?
                Op := MakeRemovedOperation(AV.VPtr)
                set Ops += array{Op}

        for (Op : Ops):
            if:
                RmOp := removed_operation[Op]
                ApplyRemovedOp[RmOp]
            else if:
                InOp := inserted_operation[Op]
                ApplyInsertOp[InOp]

    FindVertex<private>(Vertices:[]vertex, Ptr:vptr)<computes>:?vertex =
        for (V : Vertices; V.VPtr.CompareTo(Ptr) = 0):
            return option{V}
        return false

ToString<public>(Arr:dependent_array)<transacts>:string = 
    return ToString(Arr.Rga)

MakeDependentArray<public><constructor>(ReplicaId:int)<transacts> := dependent_array:
    Id := ReplicaId
    DepId := ReplicaId
    Rga := MakeRga(ReplicaId)

##########################################################
# KSTODO-1: Figure out the IDing mechanism. Since it is used
#       to order changes between dependent arrays, the IDs
#       might have impact on reproducibility of events in
#       the system
#
# KSTODO-2: Being lazy about the GetArray() calls right now
#
# KSTODO-3: Profile how this is working memory and CPU wise
##########################################################
