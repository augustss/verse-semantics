# Copyright Epic Games, Inc. All Rights Reserved.
using { /UnrealEngine.com/Temporary/SceneGraph }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org }
using { /Verse.org/Simulation }

spin_around_component := class<direct>(component):
    # Rotation Rate in degrees around the Yaw axis
    @editable
    YawRotationRate:float = 50.0
    @editable
    PitchRotationRate:float = 0.0
    @editable
    RollRotationRate:float =  0.0
    var TransformComponent:?transform_component = false
    var TickEventSubscription:?cancelable = false
    OnInitialized<override>():void=
        (super:)OnInitialized()
        set TransformComponent = option { Entity.GetComponent[transform_component] }
    OnBeginSimulation<override>():void=
        (super:)OnBeginSimulation()
    # Store UpdateRotation results on component
        set TickEventSubscription = option{ TickEvents.PrePhysics.Subscribe(UpdateRotation) }
        
    # Only set the rotation on a transform_component and keep the translation and scale the same
    (InTransformComponent:transform_component).SetRotation(Rotation:rotation):void =
        var NewTransform:transform = InTransformComponent.GetTransform()
        set NewTransform.Rotation = Rotation
        InTransformComponent.SetTransform(NewTransform)
    
    UpdateRotation(DeltaTime:float):void=
        if (TC := TransformComponent?):
            DeltaRotation:rotation = MakeRotationFromYawPitchRollDegrees(YawRotationRate * DeltaTime, PitchRotationRate * DeltaTime, RollRotationRate * DeltaTime)
            TC.SetRotation(TC.GetTransform().Rotation.RotateBy(DeltaRotation))




