# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/Verse }

positive_int := type{_X:int where 0 <= _X }

TestMethod():void=
	var X:positive_int := 0
	var Y:int := 1
	set Y = Y + 1
    if (Z := positive_int[Y]):
        set X = Z

<#
grow_only_int<public> := class:
    Initial<internal>:positive_int := 0
    var Delta<internal>:positive_int := 0
    Get():int={return Initial+Delta}
    Set(NewValue:positive_int)<transacts>:void=
        NewDelta := NewValue - Initial
        if (NewDelta >= 0):
            set Delta = NewDelta
    Inc(Increment:positive_int)<transacts>:void=
        set Delta = Delta + Increment

operator'+'<public>(lhs:grow_only_int, rhs:grow_only_int):grow_only_int=
    return grow_only_int{Initial := lhs.Initial, Delta := rhs.Get() + lhs.Delta}

operator'+'<public>(lhs:grow_only_int, val:positive_int):grow_only_int=
    return grow_only_int{Initial := lhs.Initial, Delta := val + lhs.Delta}
    #>