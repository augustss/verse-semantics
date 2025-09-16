# Copyright Epic Games, Inc. All Rights Reserved.

##########################################################
# TESTS
##########################################################

dependent_array_tests := module:

    TestSimple_ConstructRga<public>():logic =
        ExpectedId := 3
        Base:dependent_array := MakeDependentArray(ExpectedId)
        Derived:dependent_array := Base.GetDependentArray()
        if:
            # Id mechanism is set up correctly
            Base.Id = ExpectedId
            Derived.Id > Base.Id

            # Arr has dependency chain
            Base.DependentArrays[0] = Derived

            # Arr2 is in a default state
            Derived.DependentArrays.Length = 0
            Derived.BaseArray = option { Base }
        then true else false

    TestSimple_Append<public>():logic =
        Base:dependent_array := MakeDependentArray(1)
        if:
            Base.Append[MakeCrdtInt(1)]
            Base.Append[MakeCrdtInt(2)]
            Base.Append[MakeCrdtInt(3)]
            TestIntArrayEqual[Base.GetArray(), array{1,2,3}]
        then true else false

    TestSimple_Insert<public>():logic =
        Base:dependent_array := MakeDependentArray(1)
        if:
            Base.Insert[0, MakeCrdtInt(1)]
            Base.Insert[1, MakeCrdtInt(3)]
            Base.Insert[1, MakeCrdtInt(2)]
            TestIntArrayEqual[Base.GetArray(), array{1,2,3}]
        then true else false

    TestSimple_Remove<public>():logic =
        Base:dependent_array := MakeDependentArray(1)
        if:
            Base.Append[MakeCrdtInt(1)]
            TestIntArrayEqual[Base.GetArray(), array{1}]
            Base.Remove[0]
            TestIntArrayEqual[Base.GetArray(), array{}]
        then true else false

    TestSimple_Length<public>():logic =
        A:dependent_array := MakeDependentArray(1)
        if:
            A.Length() = 0
            A.Append[MakeCrdtInt(1)]
            A.Length() = 1
            A.Append[MakeCrdtInt(1)]
            A.Length() = 2
            A.Remove[0]
            A.Length() = 1
            A.Remove[0]
            A.Length() = 0
        then true else false

    TestDerived_AppendBase<public>():logic =
        A:dependent_array := MakeDependentArray(1)
        AA:dependent_array := A.GetDependentArray()
        AAA:dependent_array := AA.GetDependentArray()
        AB:dependent_array := A.GetDependentArray()

        if:
            A.Append[MakeCrdtInt(1)]
            TestIntArrayEqual[A.GetArray(), array{1}]
            TestIntArrayEqual[AA.GetArray(), array{1}]
            TestIntArrayEqual[AAA.GetArray(), array{1}]
            TestIntArrayEqual[AB.GetArray(), array{1}]
        then true else false

    TestDerived_AppendDerived<public>():logic =
        A:dependent_array := MakeDependentArray(1)
        AA:dependent_array := A.GetDependentArray()
        AAA:dependent_array := AA.GetDependentArray()
        AB:dependent_array := A.GetDependentArray()

        if:
            AA.Append[MakeCrdtInt(1)]

            TestIntArrayEqual[A.GetArray(), array{}]
            TestIntArrayEqual[AA.GetArray(), array{1}]
            TestIntArrayEqual[AAA.GetArray(), array{1}]
            TestIntArrayEqual[AB.GetArray(), array{}]
        then true else false

    TestDerived_InsertBase<public>():logic =
        A:dependent_array := MakeDependentArray(1)
        AA:dependent_array := A.GetDependentArray()
        AAA:dependent_array := AA.GetDependentArray()
        AB:dependent_array := A.GetDependentArray()

        if:
            A.Insert[0, MakeCrdtInt(1)]
            A.Insert[1, MakeCrdtInt(3)]
            A.Insert[1, MakeCrdtInt(2)]

            TestIntArrayEqual[A.GetArray(), array{1,2,3}]
            TestIntArrayEqual[AA.GetArray(), array{1,2,3}]
            TestIntArrayEqual[AAA.GetArray(), array{1,2,3}]
            TestIntArrayEqual[AB.GetArray(), array{1,2,3}]
        then true else false

    TestDerived_InsertDerived<public>():logic =
        A:dependent_array := MakeDependentArray(1)
        AA:dependent_array := A.GetDependentArray()
        AAA:dependent_array := AA.GetDependentArray()
        AB:dependent_array := A.GetDependentArray()

        if:
            AA.Insert[0, MakeCrdtInt(1)]
            AA.Insert[1, MakeCrdtInt(3)]
            AA.Insert[1, MakeCrdtInt(2)]

            TestIntArrayEqual[A.GetArray(), array{}]
            TestIntArrayEqual[AA.GetArray(), array{1,2,3}]
            TestIntArrayEqual[AAA.GetArray(), array{1,2,3}]
            TestIntArrayEqual[AB.GetArray(), array{}]
        then true else false

    TestDerived_RemoveBase<public>():logic =
        A:dependent_array := MakeDependentArray(1)
        AA:dependent_array := A.GetDependentArray()
        AAA:dependent_array := AA.GetDependentArray()
        AB:dependent_array := A.GetDependentArray()

        if:
            A.Append[MakeCrdtInt(1)]
            A.Append[MakeCrdtInt(2)]
            A.Append[MakeCrdtInt(3)]
            A.Remove[1]

            TestIntArrayEqual[A.GetArray(), array{1,3}]
            TestIntArrayEqual[AA.GetArray(), array{1,3}]
            TestIntArrayEqual[AAA.GetArray(), array{1,3}]
            TestIntArrayEqual[AB.GetArray(), array{1,3}]
        then true else false

    TestDerived_RemoveDerived<public>():logic =
        A:dependent_array := MakeDependentArray(1)
        AA:dependent_array := A.GetDependentArray()
        AAA:dependent_array := AA.GetDependentArray()
        AB:dependent_array := A.GetDependentArray()

        if:
            AA.Append[MakeCrdtInt(1)]
            AA.Append[MakeCrdtInt(2)]
            AA.Append[MakeCrdtInt(3)]
            AA.Remove[1]

            TestIntArrayEqual[A.GetArray(), array{}]
            TestIntArrayEqual[AA.GetArray(), array{1,3}]
            TestIntArrayEqual[AAA.GetArray(), array{1,3}]
            TestIntArrayEqual[AB.GetArray(), array{}]
        then true else false

    TestDerived_CreateDerivedWithExistingValuesInBase<public>():logic =
        A:dependent_array := MakeDependentArray(1)

        if:
            A.Append[MakeCrdtInt(1)]
            A.Append[MakeCrdtInt(2)]
            A.Append[MakeCrdtInt(3)]

            TestIntArrayEqual[A.GetArray(), array{1,2,3}]
            AA := A.GetDependentArray()
            TestIntArrayEqual[AA.GetArray(), array{1,2,3}]
        then true else false

    TestDerived_AddRemoveMultipleLayers<public>():logic =
        A:dependent_array := MakeDependentArray(1)
        AA:dependent_array := A.GetDependentArray()
        AAA:dependent_array := AA.GetDependentArray()

        if:
            A.Append[MakeCrdtInt(1)]
            AA.Append[MakeCrdtInt(2)]
            A.Append[MakeCrdtInt(3)]
            TestIntArrayEqual[A.GetArray(), array{1,3}]
            TestIntArrayEqual[AA.GetArray(), array{1,2,3}]
            TestIntArrayEqual[AAA.GetArray(), array{1,2,3}]

            AA.Remove[2]
            A.Append[MakeCrdtInt(4)]
            TestIntArrayEqual[A.GetArray(), array{1,3,4}]
            TestIntArrayEqual[AA.GetArray(), array{1,2,4}]
            TestIntArrayEqual[AAA.GetArray(), array{1,2,4}]
        then true else false

    TestMerge_DisconnectedBaseChanges<public>():logic = 
        A:dependent_array := MakeDependentArray(1)
        AA:dependent_array := A.GetDependentArray()
        AAA:dependent_array := AA.GetDependentArray()

        if:
            A.Append[MakeCrdtInt(1)]
            AA.Append[MakeCrdtInt(2)]

            # Forcibly disconnect AA from A. This is to mimic cases such as
            # not having AA streamed in while A is being updated
            AA.DisconnectFromBase()

            # Apply changes to both A and AA. This simulates if changes happened
            # while only one object was processing at a time, changes happened offline
            # (such as with changes to a class definition), etc.
            A.Append[MakeCrdtInt(3)]
            A.Insert[0, MakeCrdtInt(4)]
            A.Remove[1]

            # Re-attaching to the base should automatically merge the base changes
            # into AA and any arrays depending on AA
            AA.ConnectToBase(A)

            TestIntArrayEqual[A.GetArray(), array{4,3}]
            TestIntArrayEqual[AA.GetArray(), array{4,2,3}]
            TestIntArrayEqual[AAA.GetArray(), array{4,2,3}]
        then true else false

    TestMerge_DisconnectedChangesAtBothLevels<public>():logic = 
        A:dependent_array := MakeDependentArray(1)
        AA:dependent_array := A.GetDependentArray()
        AAA:dependent_array := AA.GetDependentArray()

        if:
            A.Append[MakeCrdtInt(1)]
            AA.Append[MakeCrdtInt(2)]

            # Forcibly disconnect AA from A. This is to mimic cases such as
            # not having AA streamed in while A is being updated
            AA.DisconnectFromBase()

            A.Append[MakeCrdtInt(3)]
            A.Insert[0, MakeCrdtInt(4)]
            A.Remove[1]
            AA.Append[MakeCrdtInt(5)]
            AA.Insert[0, MakeCrdtInt(6)]

            # Merge the changes from A back in, then reconnect the dependency
            AA.MergeDisconnectedChangesFromBaseArray(A)
            set A.DependentArrays = array{AA}

            TestIntArrayEqual[A.GetArray(), array{4,3}]
            TestIntArrayEqual[AA.GetArray(), array{6,4,2,5,3}]
            TestIntArrayEqual[AAA.GetArray(), array{6,4,2,5,3}]
        then true else false

##########################################################
# TEST SUITE
##########################################################

DependentArrayTestSuite():[]test_case =
    array:
        test_case{ Name := "TestSimple_ConstructRga", Func := dependent_array_tests.TestSimple_ConstructRga }

        test_case{ Name := "TestSimple_Append", Func := dependent_array_tests.TestSimple_Append }
        test_case{ Name := "TestSimple_Insert", Func := dependent_array_tests.TestSimple_Insert }
        test_case{ Name := "TestSimple_Remove", Func := dependent_array_tests.TestSimple_Remove }
        test_case{ Name := "TestSimple_Length", Func := dependent_array_tests.TestSimple_Length }

        test_case{ Name := "TestDerived_AppendBase", Func := dependent_array_tests.TestDerived_AppendBase }
        test_case{ Name := "TestDerived_AppendDerived", Func := dependent_array_tests.TestDerived_AppendDerived }
        test_case{ Name := "TestDerived_InsertBase", Func := dependent_array_tests.TestDerived_InsertBase }
        test_case{ Name := "TestDerived_InsertDerived", Func := dependent_array_tests.TestDerived_InsertDerived }
        test_case{ Name := "TestDerived_RemoveBase", Func := dependent_array_tests.TestDerived_RemoveBase }
        test_case{ Name := "TestDerived_RemoveDerived", Func := dependent_array_tests.TestDerived_RemoveDerived }
        test_case{ Name := "TestDerived_AddRemoveMultipleLayers", Func := dependent_array_tests.TestDerived_AddRemoveMultipleLayers }
        test_case{ Name := "TestDerived_CreateDerivedWithExistingValuesInBase", Func := dependent_array_tests.TestDerived_CreateDerivedWithExistingValuesInBase }

        test_case{ Name := "TestMerge_DisconnectedBaseChanges", Func := dependent_array_tests.TestMerge_DisconnectedBaseChanges }
        test_case{ Name := "TestMerge_DisconnectedChangesAtBothLevels", Func := dependent_array_tests.TestMerge_DisconnectedChangesAtBothLevels }