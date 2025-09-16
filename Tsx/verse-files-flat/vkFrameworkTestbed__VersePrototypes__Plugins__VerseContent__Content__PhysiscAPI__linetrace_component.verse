# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/SceneGraph }
using { /Verse.org/SpatialMath }

# Additional imports that you may find useful
using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/Simulation }

# A Verse-authored component that can be added to entities
@experimental
linetrace_component<public> := class<final_super>(component):

    @editable
    var Direction<public>:vector3 = vector3{Forward:=1.0, Left:=0.0, Up:=0.0}

    @editable
    var Magnitude<public>:float = 1000.0

    @editable
    var Radius<public>:float = 0.0

    var OnPrePhysicsHandle:?cancelable = false;

    OnBeginSimulation<override>():void =
        (super:)OnBeginSimulation()
        set OnPrePhysicsHandle = option{ TickEvents.PrePhysics.Subscribe(OnPrePhysics) }
                
    OnEndSimulation<override>():void =
        option{ OnPrePhysicsHandle?.Cancel() }
        (super:)OnEndSimulation()

    OnPrePhysics(Dt:float):void =
            QueryTransform := Entity.GetGlobalTransform()
            QueryTraceVector := Magnitude * (Direction * QueryTransform.Rotation)
            IgnoredEntities:[]entity = array{ Entity } + Entity.GetEntities()
            QueryVolume:collision_sphere = collision_sphere{Radius := Radius}
            for:
                TraceHit : Entity.FindSweepHits(QueryTraceVector,  QueryTransform, QueryVolume)
                HitEntityName := TraceHit.TargetComponent.Entity.GetComponent[name_component].Name or "Unknown"
                not IgnoredEntities.Find[TraceHit.TargetComponent.Entity]
            do:
                Print("Hit {HitEntityName}")
