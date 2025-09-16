# Copyright Epic Games, Inc. All Rights Reserved.

##########################################################
# TESTS
##########################################################

dependent_array_ops_based_tests := module:

    TestSimple_Append<public>():logic =
        var Base:dependent_array_ops_based := MakeDependentArrayOpsBased(1)
        if:
            set Base = Base.Append[MakeCrdtInt(1)]
            set Base = Base.Append[MakeCrdtInt(2)]
            set Base = Base.Append[MakeCrdtInt(3)]
            TestIntArrayEqual[Base.GetArray(), array{1,2,3}]
        then true else false

    TestSimple_Insert<public>():logic =
        var Base:dependent_array_ops_based := MakeDependentArrayOpsBased(1)
        if:
            set Base = Base.Insert[0, MakeCrdtInt(1)]
            set Base = Base.Insert[1, MakeCrdtInt(3)]
            set Base = Base.Insert[1, MakeCrdtInt(2)]
            TestIntArrayEqual[Base.GetArray(), array{1,2,3}]
        then true else false

    TestSimple_Remove<public>():logic =
        var Base:dependent_array_ops_based := MakeDependentArrayOpsBased(1)
        if:
            set Base = Base.Append[MakeCrdtInt(1)]
            TestIntArrayEqual[Base.GetArray(), array{1}]
            set Base = Base.Remove[0]
            TestIntArrayEqual[Base.GetArray(), array{}]
        then true else false

    TestSimple_Length<public>():logic =
        var A:dependent_array_ops_based := MakeDependentArrayOpsBased(1)
        if:
            A.Length() = 0
            set A = A.Append[MakeCrdtInt(1)]
            A.Length() = 1
            set A = A.Append[MakeCrdtInt(1)]
            A.Length() = 2
            set A = A.Remove[0]
            A.Length() = 1
            set A = A.Remove[0]
            A.Length() = 0
        then true else false

    TestDerived_AppendBase<public>():logic =
        var A:dependent_array_ops_based := MakeDependentArrayOpsBased(1)

        if:
            set A = A.Append[MakeCrdtInt(1)]
            AA:dependent_array_ops_based := A.GetDependentArray()
            AAA:dependent_array_ops_based := AA.GetDependentArray()
            AB:dependent_array_ops_based := A.GetDependentArray()
            TestIntArrayEqual[A.GetArray(), array{1}]
            TestIntArrayEqual[AA.GetArray(), array{1}]
            TestIntArrayEqual[AAA.GetArray(), array{1}]
            TestIntArrayEqual[AB.GetArray(), array{1}]
        then true else false

    TestDerived_AppendDerived<public>():logic =
        A:dependent_array_ops_based := MakeDependentArrayOpsBased(1)
        var AA:dependent_array_ops_based := A.GetDependentArray()

        if:
            set AA = AA.Append[MakeCrdtInt(1)]
            AAA:dependent_array_ops_based := AA.GetDependentArray()
            AB:dependent_array_ops_based := A.GetDependentArray()

            TestIntArrayEqual[A.GetArray(), array{}]
            TestIntArrayEqual[AA.GetArray(), array{1}]
            TestIntArrayEqual[AAA.GetArray(), array{1}]
            TestIntArrayEqual[AB.GetArray(), array{}]
        then true else false

    TestDerived_InsertBase<public>():logic =
        var A:dependent_array_ops_based := MakeDependentArrayOpsBased(1)

        if:
            set A = A.Insert[0, MakeCrdtInt(1)]
            set A = A.Insert[1, MakeCrdtInt(3)]
            set A = A.Insert[1, MakeCrdtInt(2)]
            AA:dependent_array_ops_based := A.GetDependentArray()
            AAA:dependent_array_ops_based := AA.GetDependentArray()
            AB:dependent_array_ops_based := A.GetDependentArray()

            TestIntArrayEqual[A.GetArray(), array{1,2,3}]
            TestIntArrayEqual[AA.GetArray(), array{1,2,3}]
            TestIntArrayEqual[AAA.GetArray(), array{1,2,3}]
            TestIntArrayEqual[AB.GetArray(), array{1,2,3}]
        then true else false

    TestDerived_InsertDerived<public>():logic =
        A:dependent_array_ops_based := MakeDependentArrayOpsBased(1)
        var AA:dependent_array_ops_based := A.GetDependentArray()

        if:
            set AA = AA.Insert[0, MakeCrdtInt(1)]
            set AA = AA.Insert[1, MakeCrdtInt(3)]
            set AA = AA.Insert[1, MakeCrdtInt(2)]
            AAA:dependent_array_ops_based := AA.GetDependentArray()
            AB:dependent_array_ops_based := A.GetDependentArray()

            TestIntArrayEqual[A.GetArray(), array{}]
            TestIntArrayEqual[AA.GetArray(), array{1,2,3}]
            TestIntArrayEqual[AAA.GetArray(), array{1,2,3}]
            TestIntArrayEqual[AB.GetArray(), array{}]
        then true else false

    TestDerived_RemoveBase<public>():logic =
        var A:dependent_array_ops_based := MakeDependentArrayOpsBased(1)

        if:
            set A = A.Append[MakeCrdtInt(1)]
            set A = A.Append[MakeCrdtInt(2)]
            set A = A.Append[MakeCrdtInt(3)]
            set A = A.Remove[1]
            AA:dependent_array_ops_based := A.GetDependentArray()
            AAA:dependent_array_ops_based := AA.GetDependentArray()
            AB:dependent_array_ops_based := A.GetDependentArray()

            TestIntArrayEqual[A.GetArray(), array{1,3}]
            TestIntArrayEqual[AA.GetArray(), array{1,3}]
            TestIntArrayEqual[AAA.GetArray(), array{1,3}]
            TestIntArrayEqual[AB.GetArray(), array{1,3}]
        then true else false

    TestDerived_RemoveDerived<public>():logic =
        A:dependent_array_ops_based := MakeDependentArrayOpsBased(1)
        var AA:dependent_array_ops_based := A.GetDependentArray()

        if:
            set AA = AA.Append[MakeCrdtInt(1)]
            set AA = AA.Append[MakeCrdtInt(2)]
            set AA = AA.Append[MakeCrdtInt(3)]
            set AA = AA.Remove[1]
            AAA:dependent_array_ops_based := AA.GetDependentArray()
            AB:dependent_array_ops_based := A.GetDependentArray()

            TestIntArrayEqual[A.GetArray(), array{}]
            TestIntArrayEqual[AA.GetArray(), array{1,3}]
            TestIntArrayEqual[AAA.GetArray(), array{1,3}]
            TestIntArrayEqual[AB.GetArray(), array{}]
        then true else false

    TestDerived_CreateDerivedWithExistingValuesInBase<public>():logic =
        var A:dependent_array_ops_based := MakeDependentArrayOpsBased(1)

        if:
            set A = A.Append[MakeCrdtInt(1)]
            set A = A.Append[MakeCrdtInt(2)]
            set A = A.Append[MakeCrdtInt(3)]

            TestIntArrayEqual[A.GetArray(), array{1,2,3}]
            AA := A.GetDependentArray()
            TestIntArrayEqual[AA.GetArray(), array{1,2,3}]
        then true else false

    TestDerived_AddRemoveMultipleLayers<public>():logic =
        var A:dependent_array_ops_based := MakeDependentArrayOpsBased(1)
        var AA:dependent_array_ops_based := A.GetDependentArray()
        var AAA:dependent_array_ops_based := AA.GetDependentArray()

        if:
            set A = A.Append[MakeCrdtInt(1)]
            set AA = AA.Merge(A)

            set AA = AA.Append[MakeCrdtInt(2)]

            set A = A.Append[MakeCrdtInt(3)]
            set AA = AA.Merge(A)
            set AAA = AAA.Merge(AA)

            TestIntArrayEqual[A.GetArray(), array{1,3}]
            TestIntArrayEqual[AA.GetArray(), array{1,2,3}]
            TestIntArrayEqual[AAA.GetArray(), array{1,2,3}]

            set AA = AA.Remove[2]
            set A = A.Append[MakeCrdtInt(4)]

            set AA = AA.Merge(A)
            set AAA = AAA.Merge(AA)

            TestIntArrayEqual[A.GetArray(), array{1,3,4}]
            TestIntArrayEqual[AA.GetArray(), array{1,2,4}]
            TestIntArrayEqual[AAA.GetArray(), array{1,2,4}]
        then true else false

    TestMerge_DisconnectedBaseChanges<public>():logic = 

        if:
            var A:dependent_array_ops_based := MakeDependentArrayOpsBased(1)
            set A = A.Append[MakeCrdtInt(1)]

            var AA:dependent_array_ops_based := A.GetDependentArray()
            var AAA:dependent_array_ops_based := AA.GetDependentArray()
            set AA = AA.Append[MakeCrdtInt(2)]

            # Apply changes to both A and AA. This simulates if changes happened
            # while only one object was processing at a time, changes happened offline
            # (such as with changes to a class definition), etc.
            set A = A.Append[MakeCrdtInt(3)]
            set A = A.Insert[0, MakeCrdtInt(4)]
            set A = A.Remove[1]

            # Re-attaching to the base should automatically merge the base changes
            # into AA and any arrays depending on AA
            set AA = AA.Merge(A)
            set AAA = AAA.Merge(AA)

            TestIntArrayEqual[A.GetArray(), array{4,3}]
            TestIntArrayEqual[AA.GetArray(), array{4,2,3}]
            TestIntArrayEqual[AAA.GetArray(), array{4,2,3}]
        then true else false

    TestMerge_DisconnectedChangesAtBothLevels<public>():logic = 
        if:
            var A:dependent_array_ops_based := MakeDependentArrayOpsBased(1)
            set A = A.Append[MakeCrdtInt(1)]

            var AA:dependent_array_ops_based := A.GetDependentArray()
            set AA = AA.Append[MakeCrdtInt(2)]

            set A = A.Append[MakeCrdtInt(3)]
            set A = A.Insert[0, MakeCrdtInt(4)]
            set A = A.Remove[1]
            set AA = AA.Append[MakeCrdtInt(5)]
            set AA = AA.Insert[0, MakeCrdtInt(6)]

            set AA = AA.Merge(A)
            var AAA:dependent_array_ops_based := AA.GetDependentArray()

            TestIntArrayEqual[A.GetArray(), array{4,3}]
            TestIntArrayEqual[AA.GetArray(), array{6,4,2,5,3}]
            TestIntArrayEqual[AAA.GetArray(), array{6,4,2,5,3}]
        then true else false

##########################################################
# TEST SUITE
##########################################################

DependentArrayOpsBasedTestSuite():[]test_case =
    array:
        test_case{ Name := "TestSimple_Append", Func := dependent_array_ops_based_tests.TestSimple_Append }
        test_case{ Name := "TestSimple_Insert", Func := dependent_array_ops_based_tests.TestSimple_Insert }
        test_case{ Name := "TestSimple_Remove", Func := dependent_array_ops_based_tests.TestSimple_Remove }
        test_case{ Name := "TestSimple_Length", Func := dependent_array_ops_based_tests.TestSimple_Length }

        test_case{ Name := "TestDerived_AppendBase", Func := dependent_array_ops_based_tests.TestDerived_AppendBase }
        test_case{ Name := "TestDerived_AppendDerived", Func := dependent_array_ops_based_tests.TestDerived_AppendDerived }
        test_case{ Name := "TestDerived_InsertBase", Func := dependent_array_ops_based_tests.TestDerived_InsertBase }
        test_case{ Name := "TestDerived_InsertDerived", Func := dependent_array_ops_based_tests.TestDerived_InsertDerived }
        test_case{ Name := "TestDerived_RemoveBase", Func := dependent_array_ops_based_tests.TestDerived_RemoveBase }
        test_case{ Name := "TestDerived_RemoveDerived", Func := dependent_array_ops_based_tests.TestDerived_RemoveDerived }
        test_case{ Name := "TestDerived_AddRemoveMultipleLayers", Func := dependent_array_ops_based_tests.TestDerived_AddRemoveMultipleLayers }
        test_case{ Name := "TestDerived_CreateDerivedWithExistingValuesInBase", Func := dependent_array_ops_based_tests.TestDerived_CreateDerivedWithExistingValuesInBase }

        test_case{ Name := "TestMerge_DisconnectedBaseChanges", Func := dependent_array_ops_based_tests.TestMerge_DisconnectedBaseChanges }
        test_case{ Name := "TestMerge_DisconnectedChangesAtBothLevels", Func := dependent_array_ops_based_tests.TestMerge_DisconnectedChangesAtBothLevels }