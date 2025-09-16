# Copyright Epic Games, Inc. All Rights Reserved.

solaris_test_archetype_instantiation<native><public> := class:
    
    archetype_var<native><public>:int
    
    var test_result<native><public>:logic


SolarisTestArchetypeInstantiation<public> := module:
    
    PerformTest<native_callable><public>():logic=
        a0 := solaris_test_archetype_instantiation{ archetype_var:= 2*2, test_result:=false }
        return a0.test_result