# Copyright Epic Games, Inc. All Rights Reserved.

##########################################################
# TESTS
##########################################################

TestShrinkOnlyInitialValue():logic =
    A:shrink_only_int = shrink_only_int{}
    return if (A.Get() = 0) then true else false

TestShrinkOnlyIncrement():logic =
    A:shrink_only_int = shrink_only_int{}
    A.Inc(-1)
    return if (A.Get() = -1) then true else false

TestShrinkOnlyDecrement():logic =
    A:shrink_only_int = shrink_only_int{}
    A.Dec(1)
    return if (A.Get() = -1) then true else false

TestShrinkOnlyOperatorDecrement():logic =
    A:shrink_only_int = shrink_only_int{}
    B := A - 1
    return if (B.Get() = -1) then true else false

TestShinkOnlyAdd():logic =
    A:shrink_only_int = shrink_only_int{Initial := -1}
    B:shrink_only_int = shrink_only_int{Initial := -2}
    C := A + B
    return if (C.Get() = -3) then true else false

TestSubtractGrowOnly():logic =
    A:shrink_only_int = shrink_only_int{Initial := -1}
    B:grow_only_int = grow_only_int{InitialIfNoBase := 1}
    C := A - B
    return if (C.Get() = -2) then true else false

TestSubtractPositive():logic =
    A:shrink_only_int = shrink_only_int{Initial := -1}
    B := 1
    C := A - B
    return if (C.Get() = -2) then true else false

TestShrinkOnlySet():logic =
    var A:shrink_only_int = shrink_only_int{Initial := -1}
    A.Set(-3)
    return if (A.Get() = -3) then true else false

# TODO: Write tests for failure cases


##########################################################
# TEST SUITE
##########################################################

ShrinkOnlyTestSuite():[]test_case =
    array:
        test_case{ Name := "TestShrinkOnlyInitialValue", Func := TestShrinkOnlyInitialValue }
        test_case{ Name := "TestShrinkOnlyIncrement", Func := TestShrinkOnlyIncrement }
        test_case{ Name := "TestShrinkOnlyDecrement", Func := TestShrinkOnlyDecrement }
        test_case{ Name := "TestShrinkOnlyOperatorDecrement", Func := TestShrinkOnlyOperatorDecrement }
        test_case{ Name := "TestShinkOnlyAdd", Func := TestShinkOnlyAdd }
        test_case{ Name := "TestSubtractGrowOnly", Func := TestSubtractGrowOnly }
        test_case{ Name := "TestSubtractPositive", Func := TestSubtractPositive }
        test_case{ Name := "TestShrinkOnlySet", Func := TestShrinkOnlySet }
