# Copyright Epic Games, Inc. All Rights Reserved.

using { /Verse.org/SceneGraph }
using { /Verse.org/SpatialMath }

# Additional imports that you may find useful
using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/Simulation }

# A Verse-authored component that can be added to entities
@experimental
oscillator_component := class<final_super>(component):

    @editable
    var Direction<public>:vector3 = vector3{Forward:=0.0, Left:=0.0, Up:=1.0}

    @editable
    var Amplitude<public>:float = 10.0

    @editable
    var Period<public>:float = 1.0

    @editable
    var Delay<public>:float = 0.0

    var Time:float = 0.0;

    var OnPrePhysicsHandle:?cancelable = false;
  
    OnBeginSimulation<override>():void =
        (super:)OnBeginSimulation()
        set OnPrePhysicsHandle = option{ TickEvents.PrePhysics.Subscribe(OnPrePhysics) }
                
    OnEndSimulation<override>():void =
        option{ OnPrePhysicsHandle?.Cancel() }
        (super:)OnEndSimulation()

    # velocity as function of time
    V(A: float, T:float):float = 
        Frequency := 2.0 * PiFloat / Period
        return Frequency * A * Cos(Frequency * T);
    
    VX(T:float, YUnused:float):float = 
        return V(Amplitude * Direction.Forward, T)

    VY(T:float, YUnused:float):float = 
        return V(Amplitude * Direction.Left, T)

    VZ(T:float, YUnused:float):float = 
        return V(Amplitude * Direction.Up, T)
        
    # RK4 integrate (helps cope with initial large DT)
    RungeKutta4(DYDT:tuple(float, float)->float, YN:float, TN:float, H:float):float =
        K1 := DYDT(TN, YN)
        K2 := DYDT(TN + H / 2.0, YN + H * K1 / 2.0)
        K3 := DYDT(TN + H / 2.0, YN + H * K2 / 2.0)
        K4 := DYDT(TN + H, YN + H * K3)
        return YN + (H / 6.0) * (K1 + 2.0 * K2 + 2.0 * K3 + K4)

    OnPrePhysics(Dt:float):void =
        if (Period > 0.0):
            if (TransformComponent := Entity.GetComponent[transform_component]):
                var Transform :transform = TransformComponent.GlobalTransform
                var NetTime:float = Time - Delay
                var NetDt:float = Dt;

                if (Time < Delay):
                    set NetTime = 0.0
                    set NetDt = 0.0

                NewPosition := vector3
                {
                    Forward := RungeKutta4(VX, Transform.Translation.Forward, NetTime, NetDt),
                    Left := RungeKutta4(VY, Transform.Translation.Left, NetTime, NetDt),
                    Up := RungeKutta4(VZ, Transform.Translation.Up, NetTime, NetDt)
                }
                
                set Transform.Translation = NewPosition
                set TransformComponent.GlobalTransform = Transform

                set Time = Time + Dt

