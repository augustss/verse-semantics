# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/Verse }

##########################################################
# DependentArrayOpsBased
##########################################################
dependent_array_ops_based<public> := class<final><unique>:
    ##########################################################
    # Members
    ##########################################################
    Base<internal>:?dependent_array_ops_based := false
    Rga<internal>:rga = rga{}
    Ops<internal>:[]operation = array{}

    ##########################################################
    # Operation applications
    ##########################################################
    ApplyInsertOp<private>(Op:inserted_operation)<decides><transacts>:dependent_array_ops_based =
        return dependent_array_ops_based:
            Base := Base
            Rga := Rga.ApplyInserted[Op]
            Ops := Ops + array{Op}

    ApplyRemovedOp<private>(Op:removed_operation)<decides><transacts>:dependent_array_ops_based =
        return dependent_array_ops_based:
            Base := Base
            Rga := Rga.ApplyRemoved[Op]
            Ops := Ops + array{Op}

    ##########################################################
    # Public API
    ##########################################################

    GetDependentArray<public>()<transacts>:dependent_array_ops_based = 
        # KSTODO: Better keying mechanism
        EmptyArray := MakeDependentArrayOpsBased(Self.Rga.Sequencer.ReplicaId + 1)
        return EmptyArray.Merge(Self)

    Append<public>(Value:crdt_value_interface)<decides><transacts>:dependent_array_ops_based =
        Op := Rga.CreateAppend[Value]
        return ApplyInsertOp[Op]

    Insert<public>(InsertionIndex:int, Element:crdt_value_interface)<decides><transacts>:dependent_array_ops_based =
        Op := Rga.CreateInserted[InsertionIndex, Element]
        return ApplyInsertOp[Op]

    Remove<public>(Index:int)<decides><transacts>:dependent_array_ops_based =
        Op := Rga.CreateRemoved[Index]
        return ApplyRemovedOp[Op]

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

    ##########################################################
    # Internal/Private API
    ##########################################################

    GetArray<internal>()<transacts>:[]crdt_value_interface =
        return Rga.GetArray()

    GetAllOps<internal>()<transacts>:[]operation =
        if (ValidBase := Base?):
            return ValidBase.Ops + Ops
        else:
            return Ops

    Merge<public>(BaseArray:dependent_array_ops_based)<transacts>:dependent_array_ops_based =
        # Collect all the operations starting from the root.
        # Then run them forward on the new collection.
        var AllOps:[]operation = BaseArray.GetAllOps() + Ops

        # Start from an empty array, then build it back up using just the operations
        var Result:dependent_array_ops_based = MakeDependentArrayOpsBased(BaseArray.Rga.Sequencer.ReplicaId+1)
        for (Op : AllOps):
            if:
                RmOp := removed_operation[Op]
                set Result = Result.ApplyRemovedOp[RmOp]
            else if:
                InOp := inserted_operation[Op]
                set Result = Result.ApplyInsertOp[InOp]
        return dependent_array_ops_based:
            Base := option{ BaseArray }
            Rga := Result.Rga
            Ops := Ops # only store ops from this level, not from the new base

    FindVertex<private>(Vertices:[]vertex, Ptr:vptr)<computes>:?vertex =
        for (V : Vertices; V.VPtr.CompareTo(Ptr) = 0):
            return option{V}
        return false

ToString<public>(Arr:dependent_array_ops_based)<transacts>:string = 
    return "{Arr.Rga}\n{Arr.Ops.Length}"

MakeDependentArrayOpsBased<public><constructor>(ReplicaId:int)<transacts> := dependent_array_ops_based:
    Rga := MakeRga(ReplicaId)
