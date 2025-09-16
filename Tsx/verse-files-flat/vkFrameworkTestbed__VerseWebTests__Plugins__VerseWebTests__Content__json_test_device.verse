# Copyright Epic Games, Inc. All Rights Reserved.

using { /Fortnite.com/VerseInternalTesting/VerseFramework}
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com }

json_test_device<epic_internal> := class(test_device):
    ##########################################################
    # TEST CLASS SETUP
    ##########################################################
    SuiteName<override>:string = "JSONTestSuite"

    # All tests need to be included in this array in order to be run.
    # The test category will allow for filtering tests. Currently, filters are not enforced,
    # but the `test_runner_device` specifically will run "editor" and "no_editor" tests.
    TestCases<override>():[]test_case = 
        array:
            test_case{ Name := "JSONTest_ParseObject", Func := JSONTest_ParseObject, Category := array{"editor"} }
            test_case{ Name := "JSONTest_ParseObjectContainingAllTypes", Func := JSONTest_ParseObjectContainingAllTypes, Category := array{"editor"} }
            test_case{ Name := "JSONTest_ParseArray", Func := JSONTest_ParseArray, Category := array{"editor"} }
            test_case{ Name := "JSONTest_ParseString", Func := JSONTest_ParseString, Category := array{"editor"} }
            test_case{ Name := "JSONTest_ParseFloat", Func := JSONTest_ParseFloat, Category := array{"editor"} }
            test_case{ Name := "JSONTest_ParseInt", Func := JSONTest_ParseInt, Category := array{"editor"} }
            test_case{ Name := "JSONTest_ParseNull", Func := JSONTest_ParseNull, Category := array{"editor"} }
            # enable once JSON.value.AsBool is available
            #test_case{ Name := "JSONTest_ParseTrue", Func := JSONTest_ParseTrue, Category := array{"editor"} }
            #test_case{ Name := "JSONTest_ParseFalse", Func := JSONTest_ParseFalse, Category := array{"editor"} }

    ##########################################################
    # TESTS
    ##########################################################
    JSONTest_ParseObject()<suspends>:void =
        JSONString := "\{\}"
        ObjectFieldCount:int =
            if:
                RootValue := JSON.Parse[JSONString]
                ObjectValue := RootValue.AsObject[]
            then:
                ObjectValue.Length
            else:
                -1
        TestRunner.CheckEqual(ObjectFieldCount, 0, "Parse Object")

    JSONTest_ParseObjectContainingAllTypes()<suspends>:void =
        JSONString := "\{\"object\":\{\},\"array\":[],\"string\":\"string\",\"float\":3.14,\"int\":1,\"null\":null,\"true\":true,\"false\":false\}"
        Result:logic =
            if:
                RootValue := JSON.Parse[JSONString]
                RootObjectValue := RootValue.AsObject[]
                ObjectValue := RootObjectValue["object"].AsObject[]
                ArrayValue := RootObjectValue["array"].AsArray[]
                StringValue := RootObjectValue["string"].AsString[]
                FloatValue := RootObjectValue["float"].AsFloat[]
                IntValue := RootObjectValue["int"].AsInt[]
                NullValue := RootObjectValue["null"].AsNull[]
                TrueValue := RootObjectValue["true"] # .AsBool[]
                FalseValue := RootObjectValue["false"] # .AsBool[]
            then:
                true
            else:
                false
        TestRunner.CheckTrue(Result, "Parse Object Containing All Types")
    
    JSONTest_ParseArray()<suspends>:void =
        JSONString := "[]"
        ArrayLength:int =
            if:
                RootValue := JSON.Parse[JSONString]
                ArrayValue := RootValue.AsArray[]
            then:
                ArrayValue.Length
            else:
                -1
        TestRunner.CheckEqual(ArrayLength, 0, "Parse Array")

    JSONTest_ParseString()<suspends>:void =
        JSONString := "\"string\""
        String:string =
            if:
                RootValue := JSON.Parse[JSONString]
                StringValue := RootValue.AsString[]
            then:
                StringValue
            else:
                ""
        TestRunner.CheckEqual(String, "string", "Parse String")

    JSONTest_ParseFloat()<suspends>:void =
        JSONString := "3.14"
        Number:float =
            if:
                RootValue := JSON.Parse[JSONString]
                NumberValue := RootValue.AsFloat[]
            then:
                NumberValue
            else:
                0.0
        TestRunner.CheckEqual(Number, 3.14, "Parse Number (float)")

    JSONTest_ParseInt()<suspends>:void =
        JSONString := "1"
        Number:int =
            if:
                RootValue := JSON.Parse[JSONString]
                NumberValue := RootValue.AsInt[]
            then:
                NumberValue
            else:
                0
        TestRunner.CheckEqual(Number, 1, "Parse Number (int)")

    JSONTest_ParseNull()<suspends>:void =
        JSONString := "null"
        Success:logic =
            if:
                RootValue := JSON.Parse[JSONString]
                RootValue.AsNull[]
            then:
                true
            else:
                false
        TestRunner.CheckTrue(Success, "Parse Null")

<#  # enable once JSON.value.AsBool is available
    JSONTest_ParseTrue()<suspends>:void =
        JSONString := "true"
        Value:logic =
            if:
                RootValue := JSON.Parse[JSONString]
                BoolValue := RootValue.AsBool[]
            then:
                BoolValue
            else:
                false
        TestRunner.CheckTrue(Value, "Parse true")

    JSONTest_ParseFalse()<suspends>:void =
        JSONString := "false"
        Value:logic =
            if:
                RootValue := JSON.Parse[JSONString]
                BoolValue := RootValue.AsBool[]
            then:
                BoolValue
            else:
                true
        TestRunner.CheckFalse(Value, "Parse false")
#>
