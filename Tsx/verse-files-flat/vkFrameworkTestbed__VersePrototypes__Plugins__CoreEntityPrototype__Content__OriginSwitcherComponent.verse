
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/SceneGraph }
using { /Verse.org/Simulation }

@experimental
origin_switcher_component := class<final_super>(component):
    @editable
    MoveTime<public>:float = 5.0
    @editable
    Origin1<public>:entity
    @editable
    Origin2<public>:entity

    var CurrentOrigin:?entity = false
    var AccumulatedTime:float = 0.0
    var OnUpdateCancelable:?cancelable = false

    OnBeginSimulation<override>():void =
        (super:)OnBeginSimulation()
        set CurrentOrigin = option{ Origin1 }
        set OnUpdateCancelable = option{ TickEvents.PrePhysics.Subscribe(OnUpdateOrigin) }
        Entity.SetOrigin(entity_origin{Entity:=Origin1})
        Print("origin initialized to Origin1")
    
    OnEndSimulation<override>():void =
        option{ OnUpdateCancelable?.Cancel() }
        (super:)OnEndSimulation()

    OnUpdateOrigin(DeltaTime:float):void =
        set AccumulatedTime += DeltaTime
        if (AccumulatedTime > MoveTime):
            set AccumulatedTime -= MoveTime
            if (CurrentOrigin? = Origin1):
                set CurrentOrigin = option{ Origin2 }
                Entity.SetOrigin(entity_origin{Entity:=Origin2})
                Print("origin changed to Origin2")
            else:
                set CurrentOrigin = option{ Origin1 }
                Entity.SetOrigin(entity_origin{Entity:=Origin1})
                Print("origin changed to Origin1")
