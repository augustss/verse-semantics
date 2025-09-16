# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/Concurrency }
using { /Verse.org/SceneGraph }
using { /Verse.org/SceneGraph/VerseCamera }
using { /Verse.org/SpatialMath }

# Additional imports that you may find useful
using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/Simulation }

# Derived camera component for helicopter - Switch which line is commented to switch between camera director and camera component implementations
@experimental
copter_camera_component := class<final_super>(copter_camera_component_director_version) {}
#copter_camera_component := class<final_super>(copter_camera_component_simple_version) {}

@experimental
simple_follow_camera_mode<epic_internal> := class<concrete>(camera_mode):

    # Follow offset
    @editable
    FollowOffset : vector3 = vector3{ Forward:= -1500.0, Left:= 100.0, Up:=200.0 }

    #Called by the camera mode to build up the camera settings
    Update<override>(UpdateParameters:camera_update_parameters):camera_mode_result=
        if:
            DirectorEntity := UpdateParameters.CameraDirectorComponent.Entity
            TargetComp := DirectorEntity.GetComponent[verse_prototypes_camera_target_component]
            TargetEntity := TargetComp.TargetEntity?
        then:
            TargetTransform := TargetEntity.GetGlobalTransform()
            
            Result := camera_mode_result:
                CameraRenderLocation := TargetTransform.Translation + FollowOffset
                CameraProjectionMode := camera_projection_mode.Perspective
                
            Result.SetFieldOfView(90.0)
            Result

        else:
            (super:)Update(UpdateParameters)

# Derived camera component for helicopter
@experimental
copter_camera_component_director_version := class<final_super>(camera_director_component):

    var CameraModes<override> : []camera_mode = array{simple_follow_camera_mode{}}

    # Target Entity
    @editable
    var TargetEntity : ?entity = false

    # Follow offset
    @editable
    FollowOffset : vector3 = vector3{ Forward:= -1500.0, Left:= 100.0, Up:=200.0 }

# Derived camera component for helicopter
@experimental
copter_camera_component_simple_version := class<final_super>(camera_component):

    # Target Entity
    @editable
    var TargetEntity : ?entity = false

    # Follow offset
    @editable
    FollowOffset : vector3 = vector3{ Forward:= -1500.0, Left:= 100.0, Up:=200.0 }

    # For tick
    var CameraTick:?cancelable = false

    OnBeginSimulation<override>():void =
        (super:)OnBeginSimulation()
        
        Print("Copter Control Component Init")
        set CameraTick = option{TickEvents.EndPhysics.Subscribe(UpdateCamera)}

    OnEndSimulation<override>():void =
        (super:)OnEndSimulation()

        if (CameraTickCancellable := CameraTick?):
            CameraTickCancellable.Cancel()

    UpdateCamera(DeltaTime:float):void=
        if:
            Target := TargetEntity?
        then:
            FollowEntity(Target)

    # Move the camera position to a configurable offset from the target entity
    FollowEntity(Target:entity):void=
        TargetTransform := Target.GetGlobalTransform()
        FollowTransform := transform { Translation := TargetTransform.Translation + FollowOffset }
        Entity.SetGlobalTransform(FollowTransform)

    # Look at the target entity
    LookAtEntity(Target:entity):void=
        TargetTransform := Target.GetGlobalTransform()
        LookAtLocation(TargetTransform.Translation)

    # Look at an arbitrary location
    LookAtLocation(Location:vector3):void=
        CameraTransform := Entity.GetGlobalTransform()
        