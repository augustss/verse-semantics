# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/Verse }

shrink_only_int<public> := class:
    Initial<internal>:negative_int := 0
    var Delta<internal>:negative_int := 0
    Get()<transacts>:int={return Initial+Delta}
    Set(NewValue:negative_int)<transacts>:void=
        if (NewDelta := negative_int[NewValue - Initial]):
            set Delta = NewDelta
    Inc(Increment:negative_int)<transacts>:void=
        if (NewDelta := negative_int[Delta + Increment]):
            set Delta = NewDelta
    Dec(Decrement:positive_int)<transacts>:void=
        if (NewDelta := negative_int[Delta - Decrement]):
            set Delta = NewDelta

operator'+'<public>(lhs:shrink_only_int, rhs:shrink_only_int):shrink_only_int=
    if (NewDelta := negative_int[lhs.Delta + rhs.Get()]):
        return shrink_only_int{Initial := lhs.Initial, Delta := NewDelta}
    else:
        return lhs

operator'-'<public>(lhs:shrink_only_int, rhs:grow_only_int):shrink_only_int=
    if (NewDelta := negative_int[lhs.Delta - rhs.Get()]):
        return shrink_only_int{Initial := lhs.Initial, Delta := NewDelta}
    else:
        return lhs

operator'+'<public>(lhs:shrink_only_int, val:negative_int):shrink_only_int=
    if (NewDelta := negative_int[val + lhs.Delta]):
        return shrink_only_int{Initial := lhs.Initial, Delta := NewDelta}
    else:
        return lhs

operator'-'<public>(lhs:shrink_only_int, val:positive_int):shrink_only_int=
    if (NewDelta := negative_int[lhs.Delta - val]):
        return shrink_only_int{Initial := lhs.Initial, Delta := NewDelta}
    else:
        return lhs