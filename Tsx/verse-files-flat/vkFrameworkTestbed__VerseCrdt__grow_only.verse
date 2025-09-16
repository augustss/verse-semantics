# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/Verse }

positive_int := type{_X:int where 0 <= _X }
negative_int := type{_X:int where _X <= 0 }

positive_int_deltas<internal> := class(crdt_value_interface):
    var Deltas:[int]positive_int := map{}

    Stringify<override>()<transacts>:string=
        var Result:string := ""
        for (Key->Value: Deltas):
            set Result = Result + "    UUID[{Key}]=>Delta[{Value}]\r\n"
        return Result

grow_only_int<public> := class(crdt_value_interface):
    var BaseValue<internal>:?grow_only_int := false
    var InitialIfNoBase<internal>:positive_int := 0
    var DeltaSum<internal>:positive_int := 0
    var UUID:positive_int := 0
    ReplicaId:positive_int := 0

    #TODO: Use a 2D map when SOL-2126 is completed.
    #TODO: When implemented, initialize the map by referencing other members.
    var History<private>:[int]positive_int_deltas := map{} #map{ReplicaId=>positive_int_deltas{Deltas:=map{UUID=>0}}}

    Merge(Other:grow_only_int):void=
        # Merge existing Replicas
        for:
            ReplicaKey->ReplicaValue : History
            OtherValue := Other.History[ReplicaKey]
            UUIDKey->OtherDelta : OtherValue.Deltas
            not (ReplicaValue.Deltas[UUIDKey] >= OtherDelta)
        do:
            if (set ReplicaValue.Deltas[UUIDKey] = OtherDelta){}

        # Merge new Replicas
        for:
            ReplicaKey->ReplicaValue : Other.History
            not (History[ReplicaKey])
        do:
            if (set History[ReplicaKey] = positive_int_deltas{Deltas:=ReplicaValue.Deltas}){}

        if (BaseValue?.Get() < Other.BaseValue?.Get()):
            set BaseValue = Other.BaseValue
        else if (not (BaseValue?)):
            if (OtherBase := Other.BaseValue?):
                set BaseValue = Other.BaseValue
            else if (InitialIfNoBase < Other.InitialIfNoBase):
                set InitialIfNoBase = Other.InitialIfNoBase
        set DeltaSum = GetDeltaSumFromHistory()
    
    AppendToHistory<internal>(ADelta:positive_int)<transacts>:void=
        # We could grab a new UUID on each increment if we wanted to preserve each in the history
        # if (set UUID = positive_int[UUID + 1]){}
        if (ExistingNode := History[ReplicaId]):
            if (set ExistingNode.Deltas[UUID] = positive_int[ExistingNode.Deltas[UUID] + ADelta]){}
        else:
            NewNode:positive_int_deltas := positive_int_deltas{Deltas := map{UUID=>ADelta}}
            if (set History[ReplicaId] = NewNode){}

    GetDeltaSumFromHistory<internal>():positive_int=
        var Result:positive_int := 0
        for:
            ReplicaKey->ReplicaValue : History
            Key->Value : ReplicaValue.Deltas
        do:
            if (set Result = positive_int[Result + Value]){}
        return Result

    GetInitialValue<internal>()<transacts>:int=
        if (Base := BaseValue?):
            return Base.Get()
        else:
            return InitialIfNoBase

    SetNewBase(NewBase:grow_only_int):void=
        set BaseValue = option{NewBase}

    Get<public>()<transacts>:int={return GetInitialValue()+DeltaSum}
    Set<public>(NewValue:positive_int)<transacts>:void=
        if:
            NewDelta := positive_int[NewValue - GetInitialValue()]
            DeltaToAppend := positive_int[NewDelta - DeltaSum]
        then:
            AppendToHistory(DeltaToAppend)
            set DeltaSum = NewDelta
    Inc<public>(Increment:positive_int)<transacts>:void=
        if (NewDelta := positive_int[DeltaSum + Increment]):
            AppendToHistory(Increment)
            set DeltaSum = NewDelta
    Dec<public>(Decrement:negative_int)<transacts>:void=
        if:
            NewDelta := positive_int[DeltaSum - Decrement]
            DeltaToAppend := positive_int[-Decrement]
        then:
            AppendToHistory(DeltaToAppend)
            set DeltaSum = NewDelta

    Stringify<override>()<transacts>:string=
        var Result:string := "{Get()}={GetInitialValue()}+{DeltaSum}\r\n"
        for (Key->Value : History):
            set Result = Result + " ReplicaId[{Key}]=\r\n{Value.Stringify()}\r\n"
        return Result

operator'+'<public>(lhs:grow_only_int, rhs:grow_only_int):grow_only_int=
    if (NewDelta := positive_int[rhs.Get() + lhs.DeltaSum]):
        return grow_only_int{BaseValue := lhs.BaseValue, InitialIfNoBase := lhs.InitialIfNoBase, DeltaSum := NewDelta}
    else:
        return lhs

operator'+'<public>(lhs:grow_only_int, val:positive_int):grow_only_int=
    if (NewDelta := positive_int[val + lhs.DeltaSum]):
        return grow_only_int{BaseValue := lhs.BaseValue, InitialIfNoBase := lhs.InitialIfNoBase, DeltaSum := NewDelta}
    else:
        return lhs   

operator'-'<public>(lhs:grow_only_int, rhs:shrink_only_int):grow_only_int=
    if (NewDelta := positive_int[lhs.DeltaSum - rhs.Get()]):
        return grow_only_int{BaseValue := lhs.BaseValue, InitialIfNoBase := lhs.InitialIfNoBase, DeltaSum := NewDelta}
    else:
        return lhs

operator'-'<public>(lhs:grow_only_int, val:negative_int):grow_only_int=
    if (NewDelta := positive_int[lhs.DeltaSum - val]):
        return grow_only_int{BaseValue := lhs.BaseValue, InitialIfNoBase := lhs.InitialIfNoBase, DeltaSum := NewDelta}
    else:
        return lhs 

MakeGrowOnlyInt<public><constructor>(InBaseValue:grow_only_int, InUUID:positive_int, InReplicaId:positive_int)<transacts> := grow_only_int:
    BaseValue := option{InBaseValue}
    UUID := InUUID
    ReplicaId := InReplicaId

MakeGrowOnlyInt<public><constructor>(InitialValue:positive_int, InUUID:positive_int, InReplicaId:positive_int)<transacts> := grow_only_int:
    InitialIfNoBase := InitialValue
    UUID := InUUID
    ReplicaId := InReplicaId