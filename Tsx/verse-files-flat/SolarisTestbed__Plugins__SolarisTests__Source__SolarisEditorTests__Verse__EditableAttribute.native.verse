# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/Simulation }

s0 := struct<concrete>:
    @editable
    v:logic = true

c0 := class<concrete>:
    @editable i:int = 0

e0 := enum:
    value1
    value2
    value3

c1 := class:
    @editable
    boolean:logic = false

    @editable 
    integer:int

    @editable 
    real:float

    @editable 
    str:string

    @editable
    enumeration:e0 = e0.value1

    @editable
    child_class:c0 = c0{}

    @editable
    child_struct:s0 = s0{}

    @editable 
    arr_int:[]int = array{1,2,3}

    <#
    @editable
    empty_opt_int:?int = false

    @editable
    opt_int:?int = option{123}

    @editable 
    arr_opt_int:[]?int = array{option{1},option{2},option{3}}

    @editable 
    opt_arr_opt_int:?[]?int = option{array{option{1},option{2},option{3}}}

    @editable
    opt_child_class:?c0 = option{c0{}}

    @editable
    opt_child_struct:?s0 = option{s0{}}

    @editable
    opt_boolean:?logic = option{true}

    @editable
    opt_enumeration:?e0 = option{e0.value1}

#>