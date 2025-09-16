# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/VerseCLR }

##########################################################
# TESTS
##########################################################

TestGrowOnlyInitialValue():logic =
    A:grow_only_int = grow_only_int{}
    return if (A.Get() = 0) then true else false

TestGrowOnlyIncrement():logic =
    A:grow_only_int = grow_only_int{}
    A.Inc(1)
    return if (A.Get() = 1) then true else false

TestGrowOnlyDecrement():logic =
    A:grow_only_int = grow_only_int{}
    A.Dec(-1)
    return if (A.Get() = 1) then true else false

TestGrowOnlyOperatorIncrement():logic =
    A:grow_only_int = grow_only_int{}
    B := A + 1
    return if (B.Get() = 1) then true else false

TestAdd():logic =
    A:grow_only_int = grow_only_int{InitialIfNoBase := 1}
    B:grow_only_int = grow_only_int{InitialIfNoBase := 2}
    C := A + B
    return if (C.Get() = 3) then true else false

TestSubtractShinkOnly():logic =
    A:grow_only_int = grow_only_int{InitialIfNoBase := 1}
    B:shrink_only_int = shrink_only_int{Initial := -1}
    C := A - B
    return if (C.Get() = 2) then true else false

TestSubtractNegative():logic =
    A:grow_only_int = grow_only_int{InitialIfNoBase := 1}
    B := -1
    C := A - B
    return if (C.Get() = 2) then true else false

TestSet():logic =
    var A:grow_only_int = grow_only_int{InitialIfNoBase := 1}
    A.Set(3)
    return if (A.Get() = 3) then true else false

TestMutateBase():logic =
    BaseInitialValue:positive_int := 1
    BaseUUID:positive_int := 101
    BaseReplicaId:positive_int := 1
    var Base:grow_only_int = MakeGrowOnlyInt(BaseInitialValue, BaseUUID, BaseReplicaId)

    UUID:positive_int := 1001
    ReplicaId:positive_int := 2
    var A:grow_only_int = MakeGrowOnlyInt(Base, UUID, ReplicaId)
    A.Set(3)
    Base.Set(3)
    return if (A.Get() = 5) then true else false

TestChangeBase():logic =
    BaseInitialValue:positive_int := 1
    BaseUUID:positive_int := 101
    BaseReplicaId:positive_int := 1
    var Base:grow_only_int = MakeGrowOnlyInt(BaseInitialValue, BaseUUID, BaseReplicaId)

    Base2InitialValue:positive_int := 1001
    Base2UUID:positive_int := 100001
    Base2ReplicaId:positive_int := 2
    var Base2:grow_only_int = MakeGrowOnlyInt(Base2InitialValue, Base2UUID, Base2ReplicaId)

    UUID:positive_int := 10
    ReplicaId:positive_int := 3
    var A:grow_only_int = MakeGrowOnlyInt(Base, UUID, ReplicaId)
    A.Set(3)
    Base.Set(3)
    A.SetNewBase(Base2)
    return if (A.Get() = 1003) then true else false
        
TestMerge():logic =
    BaseInitialValue:positive_int := 1
    BaseUUID:positive_int := 1
    BaseReplicaId:positive_int := 1
    var Base:grow_only_int = MakeGrowOnlyInt(BaseInitialValue, BaseUUID, BaseReplicaId)

    BaseBInitialValue:positive_int := 1001
    BaseBUUID:positive_int := 10
    BaseBReplicaId:positive_int := 2
    var BaseB:grow_only_int = MakeGrowOnlyInt(BaseBInitialValue, BaseBUUID, BaseBReplicaId)

    AUUID:positive_int := 100
    AReplicaId:positive_int := 3
    var A:grow_only_int = MakeGrowOnlyInt(Base, AUUID, AReplicaId)
    AIncrement:positive_int := 2
    if (A.Set(positive_int[BaseInitialValue + AIncrement])){}
    Base.Set(3)

    BUUID:positive_int := 1000
    BReplicaId:positive_int := 3
    var B:grow_only_int = MakeGrowOnlyInt(BaseB, BUUID, BReplicaId)
    BIncrement:positive_int := 11
    if (B.Set(positive_int[BaseBInitialValue + BIncrement])){}
    BaseB.Set(4)

    CUUID:positive_int := 10000
    CReplicaId:positive_int := 4
    var C:grow_only_int = MakeGrowOnlyInt(Base, CUUID, CReplicaId)
    CIncrement:positive_int := 5
    Base.Set(101)
    if (C.Set(positive_int[Base.Get() + CIncrement])){}

    A.Merge(B)
    A.Merge(C)
    DIncrement:positive_int := 88
    A.Inc(DIncrement)
    EIncrement:positive_int := 45
    A.Inc(EIncrement)
    DesiredValue:positive_int = if (DV := positive_int[BaseBInitialValue + AIncrement + BIncrement + CIncrement + DIncrement + EIncrement]) then DV else 1
    if (A.Get() = DesiredValue):
        Print(A.Stringify())
        return true
    else:
        Print("Expected:{DesiredValue}, Got:{A.Stringify()}")
        return false
        
TestInvalidShrinkSet():logic =
    var A:grow_only_int = grow_only_int{InitialIfNoBase := 1}
    A.Set(3)
    A.Set(1)
    return if (A.Get() = 3) then true else false

# TODO: Write tests for more failure cases



##########################################################
# TEST SUITE
##########################################################

GrowOnlyTestSuite():[]test_case =
    array:
        test_case{ Name := "TestGrowOnlyInitialValue", Func := TestGrowOnlyInitialValue }
        test_case{ Name := "TestGrowOnlyIncrement", Func := TestGrowOnlyIncrement }
        test_case{ Name := "TestGrowOnlyDecrement", Func := TestGrowOnlyDecrement }
        test_case{ Name := "TestGrowOnlyOperatorIncrement", Func := TestGrowOnlyOperatorIncrement }
        test_case{ Name := "TestAdd", Func := TestAdd }
        test_case{ Name := "TestSubtractShinkOnly", Func := TestSubtractShinkOnly }
        test_case{ Name := "TestSubtractNegative", Func := TestSubtractNegative }
        test_case{ Name := "TestSet", Func := TestSet }
        test_case{ Name := "TestMutateBase", Func := TestMutateBase }
        test_case{ Name := "TestChangeBase", Func := TestChangeBase }
        test_case{ Name := "TestMerge", Func := TestMerge }
        test_case{ Name := "TestInvalidShrinkSet", Func := TestInvalidShrinkSet }
