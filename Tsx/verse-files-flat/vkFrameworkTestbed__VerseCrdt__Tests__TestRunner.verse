# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/VerseCLR }

## TestCase to be used from a TestSuite. Func is invoked from
## the test runner, then the resulting OK/FAIL is printed based
## on the return value of Func.
##
## TODO: In the future Func would be better to be <decides>. For
## now we use a logic value since you cannot Print from a <decides>
## function, which makes debugging tests incredibly annoying.
##
## Example:
##    Test1():logic = {}
##    Test2():logic = {}
##
##    MyTestSuite():[]test_case =
##       array:
##           test_case{ Name := "Test1", Func := Test1 }
##           test_case{ Name := "Test2", Func := Test2 }
test_case<public> := struct:
    Name<public>:string
    Func<public>:(tuple()->logic)

## A collection of tests grouped together in the test output
test_suite := struct:
    Name<public>:string
    Cases:[]test_case

## Results of the run of a whole test_suite
test_suite_result := struct:
    Success:int
    Failure:int

## Executes all the test cases passed in, then reports the cummulative
## success/fail to the user. See: RunAllTests() if you are looking at
## adding new test suites.
RunTestSuite(Suite:test_suite):test_suite_result =
    Name := Suite.Name
    TestCases := Suite.Cases

    Print("[----------] {TestCases.Length} tests from {Name}")

    var FailureCount:int = 0
    for (TestCase : TestCases):
        Print("[ RUN      ] {Name}.{TestCase.Name}")
        Result := TestCase.Func()
        if (Result?):
            Print("[       OK ] {Name}.{TestCase.Name}")
        else:
            Print("[  FAILED  ] {Name}.{TestCase.Name}")
            set FailureCount += 1

    Print("[----------] {TestCases.Length} tests from {Name}")
    Print("")

    return test_suite_result:
        Success := TestCases.Length - FailureCount
        Failure := FailureCount

## Executes the passed in test suites and pretty prints the output to the console
RunTestSuites(TestSuites:[]test_suite):void =
    TestSuiteCount:int = TestSuites.Length
    var Count:int = 0; for (T : TestSuites) { set Count += T.Cases.Length }
    TestCount:int = Count

    Print("[==========] Running {TestCount} tests from {TestSuiteCount} test suites")
    Print("")

    var SuccessCount:int = 0
    var FailureCount:int = 0
    for (Suite : TestSuites):
        Result := RunTestSuite(Suite)
        set SuccessCount += Result.Success
        set FailureCount += Result.Failure

    Print("[==========] {TestCount} tests from {TestSuiteCount} test suites ran.")
    Print("[  PASSED  ] {SuccessCount} tests passed.")
    if (FailureCount > 0):
        Print("[  FAILED  ] {FailureCount} tests failed.")

## Executable that runs all the unit tests in the system. Individual functions can be run
## by changing the value passed to --target=RunAllTests on the VerseCLR command in test.bat
RunAllTests():void =
    TestSuites:[]test_suite = array:
        test_suite{ Name := "TestRga", Cases := RgaTestSuite() }
        test_suite{ Name := "TestDependentArray", Cases := DependentArrayTestSuite() }
        test_suite{ Name := "TestDependentArrayOpsBased", Cases := DependentArrayOpsBasedTestSuite() }
        test_suite{ Name := "TestGrowOnly", Cases := GrowOnlyTestSuite() }
        test_suite{ Name := "TestShrinkOnly", Cases := ShrinkOnlyTestSuite() }
        # test_suite{ Name := "TestDependentArrayInClass", Cases := DependentArrayInClassesTestSuite() }

    RunTestSuites(TestSuites)
