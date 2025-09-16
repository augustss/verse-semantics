# Copyright Epic Games, Inc. All Rights Reserved.

using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/SceneGraph }
using { /Verse.org/SceneGraph/VerseCamera }
#using { /Verse.org/SceneGraph/VerseCameraPrototype } # TEMPORARILY DISABLED
using { /Verse.org/Simulation/Tags }

# Additional imports that you may find useful
using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/Simulation }
using { /Verse.org/Assets }

# Control cameras for the copter demo
@experimental
coptergame_camera_controller_component := class<final_super>(component):
    
    var PlayerPawn : ?entity = false

    # Player's main camera
    var MainCameraComponent : ?camera_component = false

    OnSimulate<override>()<suspends>:void =
        Print("Init Camera Controller")
        race:
            loop:
                if:
                    InitGame[]
                then:
                    return
                else:
                    Sleep(0.1)
            block:
                Sleep(10.0)
                Print("Failed to initialize camera")

    InitGame()<decides><transacts>:void=
        
        InitPlayerCamera[]

    # Setup player camera - Set the default evaluation context to target the helicopter
    InitPlayerCamera()<decides><transacts>:void=
        if:
            SimulationEntity := Entity.GetSimulationEntity[]
            Players := for ( EntityWithComp : SimulationEntity.FindDescendantEntitiesWithComponent(copter_player_component)) { EntityWithComp }
            PlayerEntity := Players[0]
            CameraEntities := for (EntityWithComp : SimulationEntity.FindDescendantEntitiesWithComponent(copter_camera_component)) { EntityWithComp }
            CameraEntity := CameraEntities[0]
            CameraComp := CameraEntity.GetComponent[copter_camera_component]
            CameraComp.ActivateForPlayerAtIndex[0]
            TargetComp := CameraEntity.GetComponent[verse_prototypes_camera_target_component]
        then:
            Print("Camera initialized")
            set PlayerPawn = option{PlayerEntity}
            set CameraComp.TargetEntity = option{PlayerEntity}
            set TargetComp.TargetEntity = option{PlayerEntity}
        else:
            false?

   