# Copyright Epic Games, Inc. All Rights Reserved.

##########################################################
# TESTS
##########################################################

TestVPtrComparison():logic =
    if:
        # Equality
        MakeVPtr(0,0).CompareTo(MakeVPtr(0,0)) = 0
        MakeVPtr(1,0).CompareTo(MakeVPtr(1,0)) = 0
        MakeVPtr(0,1).CompareTo(MakeVPtr(0,1)) = 0
        MakeVPtr(0,0).CompareTo(MakeVPtr(1,0)) <> 0
        MakeVPtr(0,0).CompareTo(MakeVPtr(0,1)) <> 0

        # Less Than
        MakeVPtr(0,0).CompareTo(MakeVPtr(1,0)) = -1
        MakeVPtr(0,0).CompareTo(MakeVPtr(0,1)) = -1
        MakeVPtr(1,0).CompareTo(MakeVPtr(1,1)) = -1

        # Greater than
        MakeVPtr(1,0).CompareTo(MakeVPtr(0,0)) = 1
        MakeVPtr(0,1).CompareTo(MakeVPtr(0,0)) = 1
        MakeVPtr(1,1).CompareTo(MakeVPtr(1,0)) = 1
    then { return true } else { return false }

TestIndexOfVPtr():logic =
    Default:rga = MakeRga(77)
    Rga:rga = rga:
        Sequencer := Default.Sequencer
        Vertices := array:
            MakeTombstone(MakeVPtr(0,0))
            MakeVertex(MakeVPtr(1,1), MakeCrdtInt(1))
            MakeVertex(MakeVPtr(2,1), MakeCrdtInt(1))

    if:
        # Note: A custom CompareTo is used for VPtr, as we need to do
        #       equality based on the UUID + ReplicaId. Each container
        #       will eventually have copies of all VPtrs which should
        #       be considered the same value
        Rga.IndexOfVPtr(MakeVPtr(0,0)) = 0
        Rga.IndexOfVPtr(MakeVPtr(1,1)) = 1
        Rga.IndexOfVPtr(MakeVPtr(2,1)) = 2
    then { return true } else { return false }

TestIsTombstone():logic =
    if:
        Ptr:vptr = vptr{ UUID := 0, ReplicaId := 194834 }
        V1:vertex = vertex{ VPtr := Ptr, Value := false } 
        V1.IsTombstone()?

        V2:vertex = MakeVertex(Ptr, MakeCrdtInt(16))
        not V2.IsTombstone()?

    then { return true } else { return false }

TestRgaConstruction():logic =
    if:
        ReplicaId := 206222
        Rga := MakeRga(ReplicaId)

        # Ensure Sequencer is initialized to starting state
        Rga.Sequencer.UUID = 0
        Rga.Sequencer.ReplicaId = ReplicaId

        # Ensure that a tombstone is placed at the head
        Rga.Vertices.Length = 1
        HeadVertex := Rga.Vertices[0]
        HeadVertex.IsTombstone()?
        HeadVertex.VPtr.UUID = 0
        HeadVertex.VPtr.ReplicaId = 0 # NullId

    then { return true } else { return false }

TestRgaAppend():logic =
    if:
        var Rga:rga = MakeRga(654654)
        Op1 := Rga.CreateAppend[MakeCrdtInt(1)]
        set Rga = Rga.ApplyInserted[Op1]
        Op2 := Rga.CreateAppend[MakeCrdtInt(2)]
        set Rga = Rga.ApplyInserted[Op2]
        Op3 := Rga.CreateAppend[MakeCrdtInt(3)]
        set Rga = Rga.ApplyInserted[Op3]

        TestIntArrayEqual[Rga.GetArray(), array{1,2,3}]
    then { true } else { false }

TestRgaInsert():logic =
    if:
        var Rga:rga = MakeRga(654654)
        Op1 := Rga.CreateInserted[0, MakeCrdtInt(1)]
        set Rga = Rga.ApplyInserted[Op1]
        Op2 := Rga.CreateInserted[1, MakeCrdtInt(2)]
        set Rga = Rga.ApplyInserted[Op2]
        Op3 := Rga.CreateInserted[2, MakeCrdtInt(3)]
        set Rga = Rga.ApplyInserted[Op3]

        TestIntArrayEqual[Rga.GetArray(), array{1,2,3}]
    then { true } else { false }

TestInsertFromTwoLocations():logic =
    # Since Rga2 has a higher ReplicaId, the writes from that instance
    # will be more to the left than writes to Rga1 in cases where
    # both write to the same slot

    if:
        # time ----------------------------------->
        #              sync1                sync2
        # Rga1   [2]   [1,2]     [1,2,3]
        # Rga2   [1]   [1,2]     [1,2]     [1,2,3]

        var Rga1:rga = MakeRga(1)
        var Rga2:rga = MakeRga(2)

        Op1_1 := Rga1.CreateInserted[0, MakeCrdtInt(2)]
        Op2_1 := Rga2.CreateInserted[0, MakeCrdtInt(1)]

        set Rga1 = Rga1.ApplyInserted[Op1_1]
        set Rga2 = Rga2.ApplyInserted[Op2_1]

        # sync1
        set Rga1 = Rga1.ApplyInserted[Op2_1]
        set Rga2 = Rga2.ApplyInserted[Op1_1]

        Op1_2 := Rga1.CreateInserted[2, MakeCrdtInt(3)]
        set Rga1 = Rga1.ApplyInserted[Op1_2]

        # sync2
        set Rga2 = Rga2.ApplyInserted[Op1_2]

        TestIntArrayEqual[Rga1.GetArray(), array{1,2,3}]
        TestIntArrayEqual[Rga2.GetArray(), array{1,2,3}]
    else { return false }

    if:
        # time ----------------------------------->
        #                 sync1
        # Rga1   [2,3]   [1,2,3]
        # Rga2   [1]     [1,2,3]
        var Rga1:rga = MakeRga(1)
        var Rga2:rga = MakeRga(2)

        # Rga1 does two inserts locally before seeing any changes from Rga2
        Op1_1 := Rga1.CreateInserted[0, MakeCrdtInt(2)]
        set Rga1 = Rga1.ApplyInserted[Op1_1]
        Op1_2 := Rga1.CreateInserted[1, MakeCrdtInt(3)]
        set Rga1 = Rga1.ApplyInserted[Op1_2]

        # Rga2 does one insert locally before seeing any cahnges from Rga1
        Op2_1 := Rga2.CreateInserted[0, MakeCrdtInt(1)]
        set Rga2 = Rga2.ApplyInserted[Op2_1]

        # sync1
        set Rga2 = Rga2.ApplyInserted[Op1_1]
        set Rga2 = Rga2.ApplyInserted[Op1_2]
        set Rga1 = Rga1.ApplyInserted[Op2_1]

        TestIntArrayEqual[Rga1.GetArray(), array{1,2,3}]
        TestIntArrayEqual[Rga2.GetArray(), array{1,2,3}]
    else { return false }

    return true

TestRemove_OnlyItem():logic =
    if:
        # x = Tombstone
        # time ----------------------------------->
        #      init      
        # Rga   [1]    [x,x]

        var Rga:rga = MakeRga(1)
        set Rga = Rga.ApplyInserted[Rga.CreateInserted[0, MakeCrdtInt(1)]]

        Op := Rga.CreateRemoved[0]
        set Rga = Rga.ApplyRemoved[Op]

        TestIntArrayEqual[Rga.GetArray(), array{}]
        TestTombstones[Rga.Vertices, array{true,true}]
    then { return true } else { return false }

TestRemove_StartItem():logic =
    if:
        # x = Tombstone
        # time ----------------------------------->
        #         init      
        # Rga   [x,1,2,3]    [x,x,2,3]

        var Rga:rga = MakeRga(1)
        set Rga = Rga.ApplyInserted[Rga.CreateInserted[0, MakeCrdtInt(1)]]
        set Rga = Rga.ApplyInserted[Rga.CreateInserted[1, MakeCrdtInt(2)]]
        set Rga = Rga.ApplyInserted[Rga.CreateInserted[2, MakeCrdtInt(3)]]

        Op := Rga.CreateRemoved[0]
        set Rga = Rga.ApplyRemoved[Op]

        TestIntArrayEqual[Rga.GetArray(), array{2,3}]
        TestTombstones[Rga.Vertices, array{true,true,false,false}]
    then { return true } else { return false }

TestRemove_MiddleItem():logic =
    if:
        # x = Tombstone
        # time ----------------------------------->
        #         init      
        # Rga   [x,1,2,3]    [x,1,x,3]

        var Rga:rga = MakeRga(1)
        set Rga = Rga.ApplyInserted[Rga.CreateInserted[0, MakeCrdtInt(1)]]
        set Rga = Rga.ApplyInserted[Rga.CreateInserted[1, MakeCrdtInt(2)]]
        set Rga = Rga.ApplyInserted[Rga.CreateInserted[2, MakeCrdtInt(3)]]

        Op := Rga.CreateRemoved[1]
        set Rga = Rga.ApplyRemoved[Op]

        TestIntArrayEqual[Rga.GetArray(), array{1,3}]
        TestTombstones[Rga.Vertices, array{true, false, true, false}]
    then { return true } else { return false }

TestRemove_LastItem():logic =
    if:
        # x = Tombstone
        # time ----------------------------------->
        #         init      
        # Rga   [x,1,2,3]    [x,1,2,x]

        var Rga:rga = MakeRga(1)
        set Rga = Rga.ApplyInserted[Rga.CreateInserted[0, MakeCrdtInt(1)]]
        set Rga = Rga.ApplyInserted[Rga.CreateInserted[1, MakeCrdtInt(2)]]
        set Rga = Rga.ApplyInserted[Rga.CreateInserted[2, MakeCrdtInt(3)]]

        Op := Rga.CreateRemoved[2]
        set Rga = Rga.ApplyRemoved[Op]

        TestIntArrayEqual[Rga.GetArray(), array{1,2}]
        TestTombstones[Rga.Vertices, array{true, false, false, true}]
    then { return true } else { return false }

TestRemove_AllItemsFromFront():logic =
    if:
        # x = Tombstone
        # time ----------------------------------->
        #         init      
        # Rga   [x,1,2,3]    [x,1,2,x]

        var Rga:rga = MakeRga(1)
        set Rga = Rga.ApplyInserted[Rga.CreateInserted[0, MakeCrdtInt(1)]]
        set Rga = Rga.ApplyInserted[Rga.CreateInserted[1, MakeCrdtInt(2)]]
        set Rga = Rga.ApplyInserted[Rga.CreateInserted[2, MakeCrdtInt(3)]]

        set Rga = Rga.ApplyRemoved[Rga.CreateRemoved[0]]
        set Rga = Rga.ApplyRemoved[Rga.CreateRemoved[0]]
        set Rga = Rga.ApplyRemoved[Rga.CreateRemoved[0]]

        TestIntArrayEqual[Rga.GetArray(), array{}]
        TestTombstones[Rga.Vertices, array{true, true, true, true}]
    then { return true } else { return false }

TestAddRemoveFromMultipleSources():logic =
    if:
        # time ----------------------------------->
        #              sync1                sync2
        # Rga1   [2]   [1,2]     [1,3,2]   [1,3]
        # Rga2   [1]   [1,2]     [1]       [1,3]

        var Rga1:rga = MakeRga(1)
        var Rga2:rga = MakeRga(2)

        InsOp1_1 := Rga1.CreateInserted[0, MakeCrdtInt(2)]
        InsOp2_1 := Rga2.CreateInserted[0, MakeCrdtInt(1)]

        # Rga1 = [2], Rga2 = [1]
        set Rga1 = Rga1.ApplyInserted[InsOp1_1]
        set Rga2 = Rga2.ApplyInserted[InsOp2_1]

        # sync1
        # Rga1 = Rga2 = [1,2]
        set Rga1 = Rga1.ApplyInserted[InsOp2_1]
        set Rga2 = Rga2.ApplyInserted[InsOp1_1]

        # Rga1 = [1,3,2], Rga2 = [1]
        InsOp1_2 := Rga1.CreateInserted[1, MakeCrdtInt(3)]
        set Rga1 = Rga1.ApplyInserted[InsOp1_2]
        RemOp2_1 := Rga2.CreateRemoved[1]
        set Rga2 = Rga2.ApplyRemoved[RemOp2_1]

        # sync2
        # Rga1 = Rga2 = [1,3]
        set Rga1 = Rga1.ApplyRemoved[RemOp2_1]
        set Rga2 = Rga2.ApplyInserted[InsOp1_2]

        TestIntArrayEqual[Rga1.GetArray(), array{1,3}]
        TestIntArrayEqual[Rga2.GetArray(), array{1,3}]
        TestTombstones[Rga1.Vertices, array{true, false, false, true}]
    else { return false }

    return true

##########################################################
# Utilities
##########################################################

## Convenience function used to validate the state of a []crdt_value_interfaces 
## which are all actaully crdt_ints.
##
## Actual: MyRga.GetArray()
## Expected: array{ 0, 1, 2 }
TestIntArrayEqual(Actual:[]crdt_value_interface, Expected:[]int)<decides><transacts>:void =
    Actual.Length = Expected.Length
    for (I := 0..Actual.Length-1): 
        ActualValue := crdt_int[Actual[I]].V
        ActualValue = Expected[I]

## Convenience function used to validate the tombstone state of Rga.Vertices.
##
## Actual: Typically MyRga.Vertices
## Expected: array{ true, false, true, false}. true = TOMBSTONE, false = Value
TestTombstones(Actual:[]vertex, Expected:[]logic)<decides><transacts>:void =
    Actual.Length = Expected.Length
    for (I := 0..Actual.Length-1; Vertex := Actual[I]):
        Vertex.IsTombstone() = Expected[I]

##########################################################
# TEST SUITE
##########################################################

RgaTestSuite():[]test_case =
    array:
        test_case{ Name := "TestVPtrComparison", Func := TestVPtrComparison }
        test_case{ Name := "TestIndexOfVPtr", Func := TestIndexOfVPtr }
        test_case{ Name := "TestIsTombstone", Func := TestIsTombstone }
        test_case{ Name := "TestRgaConstruction", Func := TestRgaConstruction }
        test_case{ Name := "TestRgaAppend", Func := TestRgaAppend }
        test_case{ Name := "TestRgaInsert", Func := TestRgaInsert }
        test_case{ Name := "TestInsertFromTwoLocations", Func := TestInsertFromTwoLocations }
        test_case{ Name := "TestRemove_OnlyItem", Func := TestRemove_OnlyItem }
        test_case{ Name := "TestRemove_StartItem", Func := TestRemove_StartItem }
        test_case{ Name := "TestRemove_MiddleItem", Func := TestRemove_MiddleItem }
        test_case{ Name := "TestRemove_LastItem", Func := TestRemove_LastItem }
        test_case{ Name := "TestRemove_AllItemsFromFront", Func := TestRemove_AllItemsFromFront }
        test_case{ Name := "TestAddRemoveFromMultipleSources", Func := TestAddRemoveFromMultipleSources }
