# Copyright Epic Games, Inc. All Rights Reserved.

##########################################################
# TESTS
##########################################################

MyFirstTest():logic=
    X := 0
    return if (X = 0) then true else false
    
MySecondTest():logic=
    X := 1
    return if (X = 0) then true else false

##########################################################
# TEST SUITE
##########################################################

MyFirstTestSuite():[]test_case =
    array:
        test_case{ Name := "MyFirstTest", Func := MyFirstTest }
        test_case{ Name := "MySecondTest", Func := MySecondTest }
