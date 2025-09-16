# Copyright Epic Games, Inc. All Rights Reserved.

using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/SceneGraph }

# Additional imports that you may find useful
using { /Verse.org }
using { /Verse.org/Colors }
using { /Verse.org/Native }
using { /Verse.org/Simulation }

# A Verse-authored component that can be added to entities
@experimental
overlap_component := class<final_super>(component):

    @editable
    LightEntity:?entity = false

    var NumOverlappingComponents:int = 0
    var IndicatorLightComponent:?capsule_light_component = false

    var OnEntityEnteredHandle:?cancelable = false;
    var OnEntityExitedHandle:?cancelable = false;

    OnBeginSimulation<override>():void =
        (super:)OnBeginSimulation()

        if (MeshComponent := Entity.GetComponent[mesh_component]):
            set OnEntityEnteredHandle = option{ MeshComponent.EntityEnteredEvent.Subscribe(OnEntityEntered) }
            set OnEntityExitedHandle = option{ MeshComponent.EntityExitedEvent.Subscribe(OnEntityExited) }
        else:
            Print("overlap_component add overlap handler failed. Missing rigidbody_component?")

        if (ChildEntity := LightEntity?):
            if (ChildLightComponent := ChildEntity.GetComponent[capsule_light_component]):
                set IndicatorLightComponent = option{ ChildLightComponent }
        else:
            ChildEntities := Entity.GetEntities()
            for (ChildEntity : ChildEntities):
                if (ChildLightComponent := ChildEntity.GetComponent[capsule_light_component]):
                    set IndicatorLightComponent = option{ ChildLightComponent }

        
    OnEndSimulation<override>():void =
        option{ OnEntityEnteredHandle?.Cancel() }
        option{ OnEntityExitedHandle?.Cancel() }
        (super:)OnEndSimulation()


    OnEntityEntered(TargetEntity:entity):void =
        if (Name := TargetEntity.GetComponent[name_component].Name):
            Print("Enter {Name}")
        set NumOverlappingComponents = NumOverlappingComponents + 1
        UpdateIndicatorLight()


    OnEntityExited(TargetEntity:entity):void =
        if (Name := TargetEntity.GetComponent[name_component].Name):
            Print("Exit {Name}")
        set NumOverlappingComponents = NumOverlappingComponents - 1
        UpdateIndicatorLight()

        
    UpdateIndicatorLight():void = 
        Print("NumOverlaps={NumOverlappingComponents}")
        if (LightComponent := IndicatorLightComponent?):
            if (NumOverlappingComponents > 1):
                set LightComponent.ColorFilter = NamedColors.Red
            else if (NumOverlappingComponents > 0):
                set LightComponent.ColorFilter = NamedColors.Orange
            else:
                set LightComponent.ColorFilter = NamedColors.Green
