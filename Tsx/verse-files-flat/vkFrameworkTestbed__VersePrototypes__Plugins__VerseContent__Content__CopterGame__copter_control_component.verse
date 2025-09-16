# Copyright Epic Games, Inc. All Rights Reserved.

using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org }
using { /Verse.org/Concurrency }
using { /Verse.org/Native }
using { /Verse.org/SceneGraph }
using { /Verse.org/SceneGraph/VerseCamera }
using { /Verse.org/Simulation }
using { /Verse.org/SpatialMath }

# Implement a few useful math utils functions in Verse
CopterMath := module:

    # FInterpTo from the engine
    FInterpTo<public>(Current:float, Target:float, DeltaTime:float, InterpSpeed:float):float=
        # If no interp speed, jump to target value
        if( InterpSpeed <= 0.0 ):
            return Target

        # Distance to reach
        Dist : float = Target - Current

        # If distance is too small, just set the desired location
        if( (Dist * Dist) < 0.00000001 )
        {
            return Target
        }

        #Delta Move, Clamp so we do not over shoot.
        DeltaMove := Dist * Clamp(DeltaTime * InterpSpeed, 0.0, 1.0)
        Current + DeltaMove

    # VInterpTo from the engine
    VInterpTo<public>(Current:(/Verse.org/SpatialMath:)vector3, Target:(/Verse.org/SpatialMath:)vector3, DeltaTime:float, InterpSpeed:float):(/Verse.org/SpatialMath:)vector3=
        # If no interp speed, jump to target value
        if( InterpSpeed <= 0.0 ):
            return Target

        # Distance to reach
        Displacement : (/Verse.org/SpatialMath:)vector3 = Target - Current

        # If distance is too small, just set the desired location
        if( (Displacement.LengthSquared()) < 0.00000001 )
        {
            return Target
        }

        #Delta Move, Clamp so we do not over shoot.
        DeltaMove := Displacement * Clamp(DeltaTime * InterpSpeed, 0.0, 1.0)
        Current + DeltaMove

# Responsible for movement and update of the helicopter
# Updates the rotor speeds and automatically makes the helicopter bank in response to movement and turning
@experimental
copter_control_component := class<final_super>(component):
    
    @doc("Rotation rate of the main rotator")
    @editable
    var MainRotorRotationRateDegreesPerSecond : float = 360.0 * 4.0
    
    @doc("Rotation rate of the tail rotator")
    @editable
    var TailRotorRotationRateDegreesPerSecond : float = 360.0 * 2.0

    # Speed will be set to zero if it goes below this speed
    @doc("Helicopter speed will be set to zero if the helicopter falls below this speed when decelerating")
    @editable
    var MinSpeed : float = 10.0

    # Friction coefficient
    @editable
    @doc("Friction coefficient for the helicopter")
    var Friction : float = 0.9

    # Max acceleration rate
    @editable
    @doc("Max acceleration rate of the helicopter when input is pegged")
    var MaxAccel : float = 45.0 * 100.0

    # Max yaw rate in degrees per second
    @editable
    @doc("Max yaw rate of the helicopter when stick is pegged")
    var MaxYawRate : float = 360.0

    # Max roll
    @editable
    @doc("Max roll induced by acceleration")
    var MaxRoll : float = 20.0

    # Max pitch
    @editable
    @doc("Max pitch induced by acceleration")
    var MaxPitch : float = 15.0

    # Speed for pitch interpolation
    @editable
    @doc("Speed at which movement-based procedural pitch interpolates")
    var PitchInterpSpeed : float = 3.0
    
    # Speed for roll interpolation
    @editable
    @doc("Speed at which movement-based procedural roll interpolates")
    var RollInterpSpeed : float = 3.0

    var FakeInputTask:?task(void) = false

    # Current velocity
    var Velocity : (/Verse.org/SpatialMath:)vector3 = (/Verse.org/SpatialMath:)vector3{Forward:= 0.0, Left:= 0.0, Up:= 0.0}

    # Forward acceleration of the helicopter (will be input driven eventually)
    var ForwardInput : float = 0.0

    # Sideways acceleration of the helicopter (will be input driven eventually)
    var SidewaysInput : float = 0.0

    # Sideways acceleration of the helicopter (will be input driven eventually)
    var TurnInput : float = 0.0

    # Target pitch
    var TargetPitch : float = 0.0

     # Target roll
    var TargetRoll : float = 0.0

    # Current helicopter yaw rate
    var YawRate : float = 0.0

    # Current helicopter yaw
    var Yaw : float = 0.0

    # Current helicopter pitch
    var Pitch : float = 0.0

    # Current helicopter pitch
    var Roll : float = 0.0

    # Camera entity
    var CameraEntity : ?entity = false

    # For tick
    var OnUpdateCancelable:?cancelable = false

    OnBeginSimulation<override>():void =
        (super:)OnBeginSimulation()
        set OnUpdateCancelable = option{TickEvents.PrePhysics.Subscribe(UpdateTick)}

        Print("Copter Control Component Init")
        
        # Until we can have real input, just fake it with an async task
        Task_FakeInput := spawn { FakeInput() }
        set FakeInputTask = option{Task_FakeInput} 

    OnEndSimulation<override>():void =
        (super:)OnEndSimulation()

        if (UpdateCancelable := OnUpdateCancelable?):
            UpdateCancelable.Cancel()

        if (RunningTask := FakeInputTask?):
            spawn { RunningTask.Cancel() }
            set FakeInputTask = false

    UpdateTick(DeltaTime:float):void=

        # TEMP HACK: DeltaTime is being passed in as 0.0, so just fake delta time
        var DT : float = DeltaTime
        if:
            Abs(DT) < 0.00000001
        then:
            set DT=CopterGameConstants.FakeDeltaTime
        
        # Update speed of the helicopter's rotors
        UpdateRotorSpeeds(DT)

        # Calculate velocity
        CalcVelocity(DT)

        if:
            Copter := Entity

            CopterTransformComp := Copter.GetComponent[transform_component]
            var CopterTransform :  (/Verse.org/SpatialMath:)transform = CopterTransformComp.GlobalTransform
            Displacement : (/Verse.org/SpatialMath:)vector3 = Velocity * DT
            set CopterTransform.Translation = CopterTransform.Translation + Displacement
            set CopterTransform.Rotation = (/Verse.org/SpatialMath:)MakeRotationFromYawPitchRollDegrees(Yaw, Pitch, Roll)
        then:
            set CopterTransformComp.GlobalTransform = CopterTransform

    FakeInput()<suspends>:void=
        set ForwardInput = 1.0
        Sleep(1.0)
        set TurnInput = -0.04
        Sleep(1.0)
        set ForwardInput = 0.0
        Sleep(2.0)
        set TurnInput = 0.0
        Sleep(1.0)
        set SidewaysInput = -1.0
        set TurnInput = 0.2
        Sleep(0.5)
        set ForwardInput = 0.3
        Sleep(0.5)
        set TurnInput = 0.0
        set ForwardInput = 1.0
        set SidewaysInput = 0.0
        Sleep(2.5)
        set TurnInput = 1.0
        Sleep(0.5)
        set TurnInput = 0.0
        Sleep(1.0)
        set TurnInput = -1.0
        Sleep(1.0)
        set TurnInput = 0.0
        Sleep(4.0)
        set TurnInput = 0.7
        Sleep(0.7)
        set TurnInput = 0.0
        set ForwardInput = 0.1
        Sleep(0.2)
        set ForwardInput = 0.0
        set SidewaysInput = 0.1
        set TurnInput = 0.1

    # Get Copter Transform
    GetCopterTransform():(/Verse.org/SpatialMath:)transform=
        if:
            CopterTransformComp := Entity.GetComponent[transform_component]
        then:
            CopterTransformComp.GlobalTransform
        else:
            (/Verse.org/SpatialMath:)transform{}

    # Update speeds of the helicopter rotors
    UpdateRotorSpeeds(DeltaTime:float):void=
        for:
            MainRotor : Entity.FindDescendantEntitiesWithTag(coptergame_part_mainrotor_tag{})
            RotorComp := MainRotor.GetComponent[copter_rotor_component]
        do:
            set RotorComp.RotationSpeedDegreesPerSecond = MainRotorRotationRateDegreesPerSecond

        for:
            TailRotor : Entity.FindDescendantEntitiesWithTag(coptergame_part_tailrotor_tag{})
            RotorComp := TailRotor.GetComponent[copter_rotor_component]
        do:
            set RotorComp.RotationSpeedDegreesPerSecond = TailRotorRotationRateDegreesPerSecond

    # Calculate velocity of the helicopter
    CalcVelocity(DeltaTime:float):void=

        OldVelocity := Velocity

        CopterTransform := GetCopterTransform()
        CopterForward := (CopterTransform.Rotation.GetForwardAxis() * (/Verse.org/SpatialMath:)vector3{Forward:=1.0, Left:=1.0, Up:=0.0}).MakeUnitVector()
        CopterLeft := (CopterTransform.Rotation.GetLeftAxis() * (/Verse.org/SpatialMath:)vector3{Forward:=1.0, Left:=1.0, Up:=0.0}).MakeUnitVector()

        ForwardDir := CopterForward
        LeftDir := CopterLeft

        RawInput := vector2{ X:=SidewaysInput, Y:=ForwardInput }

        # Calculate velocity
        MoveInput := if ( InputNorm := RawInput.MakeUnitVector[]) { InputNorm } else { vector2{X:=0.0, Y:=0.0} }
        InputAccel : (/Verse.org/SpatialMath:)vector3 = MoveInput.Y * (ForwardDir * MaxAccel) + MoveInput.X * (LeftDir * MaxAccel)
        Accel := InputAccel

        NewVelocity : (/Verse.org/SpatialMath:)vector3 = OldVelocity - (OldVelocity * Friction * DeltaTime) + (Accel * DeltaTime)

        OldSpeed := OldVelocity.Length()
        NewSpeed := NewVelocity.Length()

        if (NewSpeed > MinSpeed or NewSpeed > OldSpeed):
            set Velocity = (NewVelocity + OldVelocity) * 0.5
        else:
            set Velocity = (/Verse.org/SpatialMath:)vector3{Forward:=0.0, Left:=0.0, Up:=0.0}

        # Calculate turn input
        YawAccel := TurnInput * MaxYawRate
        NewYawRate : float = YawRate - (YawRate * Friction * DeltaTime) + (YawAccel * DeltaTime)
        set YawRate = NewYawRate

        TargetRollFromYawRate := (YawRate / MaxYawRate) * -2.0 * MaxRoll
        TargetRollFromAcceleration := SidewaysInput * MaxRoll

        set TargetPitch = ForwardInput * -MaxPitch
        set TargetRoll = Clamp(TargetRollFromYawRate + TargetRollFromAcceleration, -MaxRoll, MaxRoll)

        set Pitch = CopterMath.FInterpTo(Pitch, TargetPitch, DeltaTime, PitchInterpSpeed)
        set Roll = CopterMath.FInterpTo(Roll, TargetRoll, DeltaTime, RollInterpSpeed)
        set Yaw = Yaw + (YawRate * DeltaTime)
