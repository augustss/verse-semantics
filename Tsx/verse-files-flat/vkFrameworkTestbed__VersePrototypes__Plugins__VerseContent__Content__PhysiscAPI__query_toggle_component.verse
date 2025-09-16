# Copyright Epic Games, Inc. All Rights Reserved.

using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/SceneGraph }

# Additional imports that you may find useful
using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/Simulation }

# A Verse-authored component that can be added to entities
@experimental
query_toggle_component := class<final_super>(component):

    @editable
    var OnTime<public>:float = 1.0

    @editable
    var OffTime<public>:float = 1.0

    var StateTime:float = 0.0

    var Target:?mesh_component = false

    var OnPrePhysicsHandle:?cancelable = false
  
    OnBeginSimulation<override>():void =
        (super:)OnBeginSimulation()
        set Target = option{ Entity.GetComponent[mesh_component] }
        set OnPrePhysicsHandle = option{ TickEvents.PrePhysics.Subscribe(OnPrePhysics) }
                
    OnEndSimulation<override>():void =
        option{ OnPrePhysicsHandle?.Cancel() }
        (super:)OnEndSimulation()

    OnPrePhysics(Dt:float):void =

        if (Mesh := Target?):
            set  StateTime =  StateTime + Dt
            if (Mesh.Queryable?):
                if (OnTime > 0.0 and StateTime >= OnTime):
                    Print("Disabling Query")
                    set Mesh.Queryable = false
                    set Mesh.Collidable = false
                    set Mesh.Visible = false
                    set StateTime = 0.0
            else:
                if (OffTime > 0.0 and StateTime >= OffTime):
                    Print("Enabling Query")
                    set Mesh.Queryable = true
                    set Mesh.Collidable = true
                    set Mesh.Visible = true
                    set StateTime = 0.0



