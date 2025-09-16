using { /Verse.org/SceneGraph }
using { /Verse.org/SpatialMath }
using { /Verse.org/Simulation }

@experimental
mover_component := class<final_super>(component):
    @editable
    MoveTime<public>:float = 5.0
    var AccumulatedTime:float = 0.0
    var DeltaTimeStep:float = 100.0
    var OnUpdateCancelable:?cancelable = false

    OnBeginSimulation<override>():void =
        (super:)OnBeginSimulation()
        set OnUpdateCancelable = option{ TickEvents.PrePhysics.Subscribe(OnUpdatePosition) }
    
    OnEndSimulation<override>():void =
        option{ OnUpdateCancelable?.Cancel() }
        (super:)OnEndSimulation()

    OnUpdatePosition(DeltaTime:float):void =
        var NewTransform:transform = Entity.GetGlobalTransform()
        set NewTransform.Translation.Up += DeltaTime * DeltaTimeStep
        Entity.SetGlobalTransform(NewTransform)

        set AccumulatedTime += DeltaTime
        if (AccumulatedTime > MoveTime):
            set AccumulatedTime -= MoveTime
            set DeltaTimeStep *= -1.0
