# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/SceneGraph }
using { /Verse.org/SpatialMath }
using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/Simulation }

# This component prints begin/end overlaps as they happen
# and a list of all touching components every few seconds
collision_reporter_component<public> := class<final><final_super>(component):

    @editable
    ReportPeriod:float = 3.0

    var BodyComponent:?mesh_component = false

    var OnCollisionEnterHandle:?cancelable = false;
    var OnCollisionExitHandle:?cancelable = false;
    var OnCollisionHandle:?cancelable = false;

    var ComponentName:string = "Unknown"
    var CollidingComponents:[string]int = map{}

    OnBeginSimulation<override>():void =
        (super:)OnBeginSimulation()

        if (NameComponent := Entity.GetComponent[name_component]):
            set ComponentName = NameComponent.Name

        set BodyComponent = option{ Entity.GetComponent[mesh_component] }
        set OnCollisionEnterHandle = option{ BodyComponent?.CollisionEnteredEvent.Subscribe(OnCollisionEnter) }
        set OnCollisionExitHandle = option{ BodyComponent?.CollisionExitedEvent.Subscribe(OnCollisionExit) }
        set OnCollisionHandle = option{ BodyComponent?.CollisionEvent.Subscribe(OnCollision) }

    OnSimulate<override>()<suspends>:void =
        Print("OnSimulate: {ComponentName}")
        loop:
            Sleep(ReportPeriod)
            # NOTE: debug text draws newest at the top, so I'm printing the
            # heading last so it reads better. Bit weird, but it works.
            for (Name -> Count : CollidingComponents):
                Print("    {ComponentName} - {Name}")
            Print("{ComponentName}: {CollidingComponents.Length} Collisions")

    OnEndSimulation<override>():void =
        option{ OnCollisionEnterHandle?.Cancel() }
        option{ OnCollisionExitHandle?.Cancel() }
        option{ OnCollisionHandle?.Cancel() }
        (super:)OnEndSimulation()

    
    OnCollisionEnter(CollisionEnters:generator(collision_hit)):void = 
        for (CollisionEnter : CollisionEnters):
            var SourceName:string = CollisionEnter.CollisionVolumes.SourceComponent.Entity.GetComponent[name_component].Name or "Unknown"
            var TargetName:string = CollisionEnter.CollisionVolumes.TargetComponent.Entity.GetComponent[name_component].Name or "Unknown"
            var CurrentCount:int = (CollidingComponents[TargetName] or 0) + 1
            option{ set CollidingComponents[TargetName] = CurrentCount }
            
            Print("CollisionEnter {SourceName} - {TargetName} {CurrentCount}")

    
    OnCollisionExit(CollisionExits:generator(collision_volume_pair)):void = 
        for (CollisionExit : CollisionExits):
            var SourceName:string = CollisionExit.SourceComponent.Entity.GetComponent[name_component].Name or "Unknown"
            var TargetName:string = CollisionExit.TargetComponent.Entity.GetComponent[name_component].Name or "Unknown"
            var CurrentCount:int = (CollidingComponents[TargetName] or 1) - 1
            option{ set CollidingComponents[TargetName] = CurrentCount }
    
            Print("CollisionExit {SourceName} - {TargetName} {CurrentCount}")
            

    OnCollision(CollisionHits:generator(collision_hit)):void = 
        for (CollisionHit : CollisionHits):
            var SourceName:string = CollisionHit.CollisionVolumes.SourceComponent.Entity.GetComponent[name_component].Name or "Unknown"
            var TargetName:string = CollisionHit.CollisionVolumes.TargetComponent.Entity.GetComponent[name_component].Name or "Unknown"
            var CurrentCount:int = CollidingComponents[TargetName] or 0

            Print("Collision {SourceName} - {TargetName} {CurrentCount}")
            