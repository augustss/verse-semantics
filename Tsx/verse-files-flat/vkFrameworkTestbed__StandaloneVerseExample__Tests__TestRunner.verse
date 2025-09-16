# Copyright Epic Games, Inc. All Rights Reserved.

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

## Executes all the test cases passed in, then reports the cummulative
## success/fail to the user. See: RunAllTests() if you are looking at
## adding new test suites.
RunTestSuite(Name:string, TestCases:[]test_case):void =
    Print("[==========]")

    var FailureCount:int = 0
    for (TestCase : TestCases):
        Print("[ RUN      ] {Name}.{TestCase.Name}")
        Result := TestCase.Func()
        if (Result?):
            Print("[       OK ] {Name}.{TestCase.Name}")
        else:
            Print("[  FAILED  ] {Name}.{TestCase.Name}")
            set FailureCount += 1

    Print("[==========] {TestCases.Length} tests from {Name} ran.")
    Print("[  PASSED  ] {TestCases.Length - FailureCount} tests.")
    if (FailureCount > 0):
        Print("[  FAILED  ] {FailureCount} tests.")

## Executable that runs all the unit tests in the system. Individual functions can be run
## by changing the value passed to --target=RunAllTests on the VerseCLR command in test.bat
RunAllTests():void =
    RunTestSuite("MyFirstTestSuite", MyFirstTestSuite())
