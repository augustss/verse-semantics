using { /Verse.org }
using { /Verse.org/Native }
using { /Verse.org/SceneGraph }
using { /Verse.org/Simulation }
using { /Verse.org/SpatialMath }

# A Verse-authored component that can be added to entities
scaler_component<public> := class<final_super>(component):

    @editable
    Scale<private>:vector3 = vector3{Forward:=1.0, Left:=1.0, Up:=1.0}
    
    @editable
    Delay<private>:float = 10.0

    var Countdown:float = 0.0
  
    var OnPrePhysicsHandle:?cancelable = false
  
    OnBeginSimulation<override>():void =
        (super:)OnBeginSimulation()
        set Countdown = Delay
        set OnPrePhysicsHandle = option{ TickEvents.PrePhysics.Subscribe(OnPrePhysics) }
                
    OnEndSimulation<override>():void =
        option{ OnPrePhysicsHandle?.Cancel() }
        (super:)OnEndSimulation()

    OnPrePhysics(Dt:float):void =
        if (Countdown > 0.0):
            set Countdown = Countdown - Dt
            if (Countdown <= 0.0):
                set Countdown = 0.0
                ApplyScale()

    ApplyScale():void =
            NewTransform := transform:
                Translation := Entity.GetGlobalTransform().Translation
                Rotation := Entity.GetGlobalTransform().Rotation
                Scale := Scale
            Entity.SetGlobalTransform(NewTransform)


