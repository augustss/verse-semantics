# Copyright Epic Games, Inc. All Rights Reserved.

using { /UnrealEngine.com/VersePrototypeAssets/PrototypeAssets }
using { /Verse.org }
using { /Verse.org/Assets }
using { /Verse.org/Native }
using { /Verse.org/SceneGraph }
using { /Verse.org/SceneGraph/VerseCamera }
using { /Verse.org/Simulation }
using { /Verse.org/Simulation/Tags }
using { /Verse.org/SpatialMath }

CopterGameConstants := module:
    FakeDeltaTime<public> : float = 1.0 / 120.0

copter_rotor_axis<epic_internal> := enum:
    Yaw,
    Pitch,
    Roll

# Doesn't really do anything, it's just that gameplay tags are being a bit weird in VersePrototypes right now and I need a way to find the copter entity
@experimental
copter_player_component<epic_internal> := class<final_super>(component) {}

# Simple helicopter rotor. Makes the owning entity rotate along the specified axis at the specified rate
@experimental
copter_rotor_component<epic_internal> := class<final_super>(component):

    # Rotation axis
    @editable
    var RotationAxis<public> : copter_rotor_axis = copter_rotor_axis.Yaw

    # Current rotation speed
    @editable
    var RotationSpeedDegreesPerSecond<public> : float = 0.0

    # Current rotation
    var CurrentRotationRads : float = 0.0

    # For tick
    var OnUpdateCancelable:?cancelable = false

    OnBeginSimulation<override>():void =
        (super:)OnBeginSimulation()
        set OnUpdateCancelable = option { TickEvents.PrePhysics.Subscribe(UpdateTick) }

    OnSimulate<override>()<suspends>:void =
        Sleep(1.0)

        # CopterSoundComponent := sound_component:
        #     Entity := Self.Entity
        #     Sound := Sounds.Helicopter_SFX_MSS
        # Entity.AddComponents(array{ CopterSoundComponent })
        # CopterSoundComponent.Play()

    OnEndSimulation<override>():void =
        (super:)OnEndSimulation()
        if (TickCancelable := OnUpdateCancelable?):
            TickCancelable.Cancel()

    UpdateTick(DeltaTime:float):void =
        PiOverOneEighty := PiFloat / 180.0

        RotorRotationDelta := (RotationSpeedDegreesPerSecond * PiOverOneEighty) * DeltaTime
        set CurrentRotationRads += RotorRotationDelta

        UseLocalTransform := false
        
        if:
            UseLocalTransform?
        then:
            var RotorTransform : transform = Entity.GetLocalTransform()
            case(RotationAxis):
                copter_rotor_axis.Yaw =>  set RotorTransform.Rotation = RotorTransform.Rotation * MakeRotationFromYawPitchRollRadians(RotorRotationDelta, 0.0, 0.0)
                copter_rotor_axis.Pitch => set RotorTransform.Rotation = RotorTransform.Rotation * MakeRotationFromYawPitchRollRadians(0.0, RotorRotationDelta, 0.0)
                copter_rotor_axis.Roll => set RotorTransform.Rotation = RotorTransform.Rotation * MakeRotationFromYawPitchRollRadians(0.0, 0.0, RotorRotationDelta)
            <#
            if:
                YawPitchRoll := RotorTransform.Rotation.GetYawPitchRollDegrees()
                Yaw := YawPitchRoll[0]
                Pitch := YawPitchRoll[1]
                Roll := YawPitchRoll[2]
            then:
                case(RotationAxis):
                    copter_rotor_axis.Yaw =>   Print("Rotor Rotation: Yaw:{Yaw}")
                    copter_rotor_axis.Pitch =>  Print("Rotor Rotation: Pitch:{Pitch}")
                    copter_rotor_axis.Roll =>  Print("Rotor Rotation: Roll:{Roll}")
            #>

            Entity.SetLocalTransform(RotorTransform)
        else:
            # Note: We should be using local transform here but that doesn't seem to work right now
            var RotorTransform : transform = Entity.GetGlobalTransform()
            case(RotationAxis):
                copter_rotor_axis.Yaw =>  set RotorTransform.Rotation = RotorTransform.Rotation * MakeRotationFromYawPitchRollRadians(RotorRotationDelta, 0.0, 0.0)
                copter_rotor_axis.Pitch => set RotorTransform.Rotation = RotorTransform.Rotation * MakeRotationFromYawPitchRollRadians(0.0, RotorRotationDelta, 0.0)
                copter_rotor_axis.Roll => set RotorTransform.Rotation = RotorTransform.Rotation * MakeRotationFromYawPitchRollRadians(0.0, 0.0, RotorRotationDelta)
            
            Entity.SetGlobalTransform(RotorTransform)

# Main rotor
@experimental
copter_main_rotor_component<epic_internal> := class<final_super>(copter_rotor_component):
    var RotationAxis<override> : copter_rotor_axis = copter_rotor_axis.Yaw
    var RotationSpeedDegreesPerSecond<override> : float = 1200.0

# Main rotor
@experimental
copter_tail_rotor_component<epic_internal> := class<final_super>(copter_rotor_component):
    var RotationAxis<override> : copter_rotor_axis = copter_rotor_axis.Pitch
    var RotationSpeedDegreesPerSecond<override> : float = 1200.0
