# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/Colors }
using { /Verse.org/Native }
using { /Verse.org/Restricted }
using { /UnrealEngine.com/Tests/SolarisTestbedScripts/SolarisTestbed }

MakeAnyFromColor<public><native>(:color)<computes>:any

<# #############################################################################
 # For testing compile/link breaks.
 #>

<##
 # Uncomment this to break compilation w/ a semantic error.
 cook_test_c_c_c_compile_breaker := class:
    f():void={ v:cook_test_script_class = 42 }
 # #>

<##
# Uncomment this to break linking w/ a binding error.

cook_test_link_breaker<native> := class:
    
    F<native>():void=
# #>

<# #############################################################################
 # For testing script references, which don't require any dynamic-linking.
 #>

cook_test_script_class := class {}

<# #############################################################################
 # For testing runtime-linked (natively bound) script classes.
 #>


cook_test_script_actor<native> := class(cook_test_actor) {}

<# #############################################################################
 # For testing script that requires dynamic-linking, resolved at cook-time.
 #>

@import_as("/Script/SolarisTestbed.ASolarisTestCube")
cook_test_actor<native> := class {}


cook_test_enum<native> := enum:
    One
    Two
    Three

cook_test_tuple := tuple(int, cook_test_enum)

cook_test_parametric_type(t:type) := class{X:t}

cook_test_import_patching := class:
    ObjProp:cook_test_actor
    ClassProp:subtype(cook_test_actor)

SuspendsFunctionThatModifiesArray()<suspends>:void =
    var Array:[]int = (0, 0)
    if:
        set Array[0] = 1

CookTestImportPatching := module:
    
    DoThing<native_callable><public>(Obj:cook_test_actor):?cook_test_script_actor=
        # Test MakeDynamicallyTypedValue in C++
        MakeAnyFromColor(color{R:=1.0, G:=0.5, B:=0.1})

        # Test indexing into a string (SOL-3732)
        MyString := "Test string"
        MyArray := for (i:=0..MyString.Length; c := MyString[i]) do c

        spawn{SuspendsFunctionThatModifiesArray()}

        option{cook_test_script_actor[Obj]}

    
    EnumToInt<native_callable><public>(E:cook_test_enum):int=
        case(E):
            cook_test_enum.One   => 1
            cook_test_enum.Two   => 2
            cook_test_enum.Three => 3
    
    IntToEnum<native_callable><public>(I:int):cook_test_enum=
        case(I):
            1 => cook_test_enum.One
            2 => cook_test_enum.Two
            _ => cook_test_enum.Three

    EnumToTuple<native_callable><public>(E:cook_test_enum):cook_test_tuple=(EnumToInt(E), E)

    TestParametricType<native_callable><public>(I:int):int=
        Test := cook_test_parametric_type(int){X:=I}
        return Test.X

cook_test_has_sub_obj<native> := class:
    MySo<native>:cook_test_sub_object = cook_test_sub_object{DataValue:=42}
    TrySubObj<native_callable>():logic=
        return(MySo.DoThing())


cook_test_sub_object<native> := class:
    DoThing():logic={ return logic{DataValue = 42} }
    DataValue:int=11
