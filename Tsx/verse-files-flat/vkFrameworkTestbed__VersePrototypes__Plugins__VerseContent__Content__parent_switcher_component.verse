using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/SceneGraph }
using { /Verse.org/Simulation }

# A Verse-authored component that can be added to entities
parent_switcher_component<public> := class<final_super>(component):
    @editable
    MoveTime<public>:float = 5.0
    @editable
    Parent1<public>:entity
    @editable
    Parent2<public>:entity

    var CurrentParent:?entity = false
    var AccumulatedTime:float = 0.0
    var OnUpdateCancelable:?cancelable = false

    OnBeginSimulation<override>():void =
        (super:)OnBeginSimulation()
        set CurrentParent = option{ Parent1 }
        set OnUpdateCancelable = option{ TickEvents.PrePhysics.Subscribe(OnUpdateParent) }
        Parent1.AddEntities(array{Entity})
        Print("parent setup to Parent1")
    
    OnEndSimulation<override>():void =
        option{ OnUpdateCancelable?.Cancel() }
        (super:)OnEndSimulation()

    OnUpdateParent(DeltaTime:float):void =
        set AccumulatedTime += DeltaTime
        if (AccumulatedTime > MoveTime):
            set AccumulatedTime -= MoveTime
            if (CurrentParent?=Parent1):
                set CurrentParent = option{ Parent2 }
                Parent2.AddEntities(array{Entity})
                Print("parent changed to Parent2")
            else:
                set CurrentParent = option{ Parent1 }
                Parent1.AddEntities(array{Entity})
                Print("parent changed to Parent1")

