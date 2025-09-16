# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/SceneGraph }
using { /Verse.org/SpatialMath }

# Additional imports that you may find useful
using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/Simulation }

# A Verse-authored component that can be added to entities
@experimental
componenttrace_component<public> := class<final_super>(component):

    #@editable
    var TraceEntity<public>:?entity = false
    var TraceTransformComponent:?transform_component = false

    @editable
    var Direction<public>:vector3 = vector3{Forward:=1.0, Left:=0.0, Up:=0.0}

    @editable
    var Magnitude<public>:float = 1000.0

    var OnPrePhysicsHandle:?cancelable = false;

    OnBeginSimulation<override>():void =
        (super:)OnBeginSimulation()
        set OnPrePhysicsHandle = option{ TickEvents.PrePhysics.Subscribe(OnPrePhysics) }

        set TraceEntity = option{ Entity.GetEntities()[0] }
        set TraceTransformComponent = option{ TraceEntity?.GetComponent[transform_component] }
                
    OnEndSimulation<override>():void =
        option{ OnPrePhysicsHandle?.Cancel() }
        (super:)OnEndSimulation()

    OnPrePhysics(Dt:float):void =
        if (QueryEntity := TraceEntity?):
            QueryEntityTransform:transform = transform:
                Translation := Entity.GetGlobalTransform().Translation + GetFirstSweepHitTranslation()
                Rotation := QueryEntity.GetGlobalTransform().Rotation
            QueryEntity.SetGlobalTransform(QueryEntityTransform)

    GetFirstSweepHitTranslation():vector3 = 
        TracerTransform := Entity.GetGlobalTransform()
        QueryTraceDirection:vector3 = Direction * TracerTransform.Rotation
        QueryTraceVector:vector3 = Magnitude * QueryTraceDirection

        var FirstSweepHitDistance:float = Magnitude
        for:
            QueryEntity : TraceEntity?.FindDescendantEntitiesWithComponent(mesh_component)
            QueryEntitySweepHitDistance := GetEntityFirstSweepHitTranslation(QueryEntity, QueryTraceVector, TraceEntity?)
            QueryEntitySweepHitDistance < FirstSweepHitDistance
        do:
            set FirstSweepHitDistance = QueryEntitySweepHitDistance
            #
            #
            # TODO: we are missing some basic math to support sweeping the entity hierarchy
            # See ConcatenateTransforms and GetRelativeTransform
            # For now, just take the first hit on the root entity
            #
            #
            return FirstSweepHitDistance * QueryTraceDirection
        
        return FirstSweepHitDistance * QueryTraceDirection

    GetEntityFirstSweepHitTranslation(QueryEntity:entity, QueryTraceVector:vector3, QueryRootEntity:entity)<transacts>:float = 

        QueryRootStartTransform:transform = transform:
            Translation := Entity.GetGlobalTransform().Translation
            Rotation := QueryEntity.GetGlobalTransform().Rotation

        QueryRelativeTransform := GetRelativeTransform(QueryRootEntity.GetGlobalTransform(), QueryEntity.GetGlobalTransform());

        QueryStartTransform := ConcatenateTransforms(QueryRootStartTransform, QueryRelativeTransform)

        for:
            Hit : QueryEntity.FindSweepHits(QueryTraceVector, QueryStartTransform)
        do:
            return Hit.SourceHitDistance

        return Magnitude

    ConcatenateTransforms(L:transform, R:transform)<transacts>:transform =
        return L # TODO
    
    GetRelativeTransform(ParentTransform:transform, ChildTransform:transform)<transacts>:transform =
        return transform{}  # TODO
    


        

