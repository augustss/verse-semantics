# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/Verse }

##########################################################
# vertex
##########################################################

vertex<public> := class<final><unique>:
    VPtr<public>:vptr
    Value<public>:?crdt_value_interface

    IsTombstone<public>()<transacts>:logic = 
        if (V := Value?) { return false } else { return true }

MakeVertex<public><constructor>(VPtr:vptr, Value:crdt_value_interface)<transacts> := vertex:
    VPtr := VPtr
    Value := option { Value }

MakeTombstone<public><constructor>(VPtr:vptr)<transacts> := vertex:
    VPtr := VPtr
    Value := false

ToString<public>(Vertex:vertex)<transacts>:string =
    Value := if (V := Vertex.Value?) { V.Stringify() } else { "TOMBSTONE" }
    return "Vptr: ({Vertex.VPtr}) | Value: ({Value})"

##########################################################
# RGA
##########################################################

## Replicated Growable Array
##
## Reference Papers:
##    - https://bartoszsypytkowski.com/operation-based-crdts-arrays-1/
##    - https://replicated.cc/rdts/rga/
rga<public> := class<final>:

    #######################
    # Members
    #######################
    Sequencer<internal>:vptr = vptr{}
    Vertices<internal>:[]vertex = array{}

    #######################
    # Methods
    #######################

    ## Finds the first valid index into the Vertex array. 
    ## This skips over any tombstoned values. If the entire
    ## array is tombstones this will return Vertices.Length.
    FindFirstIndexWithTombstones<private>()<transacts>:int =
        for (I := 0..Vertices.Length-1):
            if (Vertex := Vertices[I]; not Vertex.IsTombstone()?):
                return I
        return Vertices.Length

    # Maps user-given index (which ignores tombstones) into physical index inside of `vertices` array.
    IndexWithTombstones<private>(Index:int)<transacts>:int =
        var Remaining:int = Index

        var Offset:int = FindFirstIndexWithTombstones()
        loop:
            if (Remaining = 0):
                return Offset

            if (Vertex := Vertices[Offset]; Vertex.IsTombstone()?):
                set Offset += 1
            else:
                set Offset += 1
                set Remaining -= 1

    IndexOfVPtr(VPtr:vptr)<transacts>:int =
        for (I := 0..Vertices.Length-1):
            if (Vertices[I].VPtr.CompareTo(VPtr) = 0):
                return I
        return -1

    Shift<internal>(Offset:int, Ptr:vptr)<transacts>:int =
        var InnerOffset:int = Offset
        loop:
            if (InnerOffset >= Vertices.Length):
                return InnerOffset
            if (Successor := Vertices[InnerOffset].VPtr):
                if (Successor.CompareTo(Ptr) < 0):
                    return InnerOffset
            set InnerOffset += 1

    NextSequencer(VPtr:vptr)<transacts>:vptr =
        return MakeVPtr(VPtr.UUID+1, VPtr.ReplicaId)

    GetLastIndex()<transacts>:int =
        var Index:int = 0
        for (A : Vertices; Value := A.Value?) { set Index += 1 }
        return Index

    CreateAppend(Value:crdt_value_interface)<decides><transacts>:inserted_operation =
        LastIndex := GetLastIndex()
        return CreateInserted[LastIndex, Value]

    CreateInserted(InsertIndex:int, Value:crdt_value_interface)<decides><transacts>:inserted_operation =
        Index := IndexWithTombstones(InsertIndex) # start from 1 to skip header vertex
        Prev := Vertices[Index-1].VPtr # get VPtr of previous element or RGA's head
        At := NextSequencer(Sequencer)
        MakeInsertedOperation(Prev, At, Value)

    CreateRemoved(RemoveIndex:int)<decides><transacts>:removed_operation =
        Index := IndexWithTombstones(RemoveIndex)
        At := Vertices[Index].VPtr # get VPtr of a previous element
        MakeRemovedOperation(At)

    ApplyInserted<public>(Op:inserted_operation)<decides><transacts>:rga =
        Predecessor := Op.After
        Ptr := Op.At
        Value := Op.Value

        # Find index where Predecessor vertex can be found
        PredecessorIdx := IndexOfVPtr(Predecessor)

        # Adjust index where new vertex is to be inserted
        InsertIdx := Shift(PredecessorIdx+1, Ptr)
        NewVertices := Vertices.Insert[InsertIdx, array{MakeVertex(Ptr, Value)}]

        # Update RGA to store the highest observed sequence number 
        # (a.k.a. Lamport timestamp)
        SeqNr := Sequencer.UUID
        ReplicaId := Sequencer.ReplicaId

        return rga:
            Sequencer := MakeVPtr(Max(Ptr.UUID, SeqNr), ReplicaId)
            Vertices := NewVertices

    ApplyRemoved(Op:removed_operation)<decides><transacts>:rga =
        # Find index where removed vertex can be found and clear its content to tombstone it
        Index := IndexOfVPtr(Op.At)
        At := Vertices[Index].VPtr
        NewVertices := Vertices.ReplaceElement[Index, MakeTombstone(At)]
        return rga{ Sequencer := Sequencer, Vertices := NewVertices }

    ## Computes an array from the CRDT collection. This iterates through
    ## internal structures to produce the array which could become costly.
    ## Be careful not to call this multiple times when the collection isn't
    ## being changed.
    GetArray()<transacts>:[]crdt_value_interface =
        return for (A : Vertices; Value := A.Value?) { Value }

MakeRga<public><constructor>(ReplicaId:int)<transacts> := rga:
    let:
        HeadVPtr := MakeVPtr(0, 0)
        HeadVertex := MakeTombstone(HeadVPtr)

    Sequencer := MakeVPtr(0, ReplicaId)
    Vertices := array{ HeadVertex }

## Produces a comma separated representation of the bwra values.
## 
## Example: Print("{MyRga}")
ToString<public>(Rga:rga)<transacts>:string=
    var Result:string = ""
    set Result += "\n       ["
    set Result += "\n           Sequencer: {Rga.Sequencer}"

    Vertices := Rga.Vertices
    for (I := 0..Vertices.Length-1):
        if (Vertex := Vertices[I]):
            set Result += "\n           {Vertex}"

    set Result += "\n       ]"
    return Result
