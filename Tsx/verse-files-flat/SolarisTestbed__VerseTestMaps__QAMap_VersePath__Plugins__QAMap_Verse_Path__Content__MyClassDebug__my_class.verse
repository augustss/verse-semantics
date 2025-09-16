using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Fortnite.com/Devices }
using { /Fortnite.com/Devices/CreativeAnimation }
using { /Verse.org/Simulation }

base_module <public> := module:
    sub_module <public> := module:
        mini_module <public> := module:
            my_class <public> := class:
                var <private> MyInt : int = 0
                FooFunc<public>():void=
                    return Print ("[SUCCESS] FooFunc() | mini_module.my_class")

            # my_interface <private> := interface():

            animation_device := class(creative_device):
                @editable ControlledProps: []creative_prop = array{}
                @editable MovementVector: vector3 = vector3{X:=0.0,Y:=0.0,Z:=500.0}
            
                OnBegin<override>()<suspends>:void=
                    Meshes.Planet
                    for (ControlledProp : ControlledProps):
            
                        AnimateProp(ControlledProp, MovementVector)
            
                AnimateProp(Prop : creative_prop, MovementDistance: vector3) : void =
                    if (AController := Prop.GetAnimationController[]):
                            AController.SetAnimation(GetKeyFrame(MovementDistance), ?Mode:=animation_mode.PingPong)
                            AController.Play()
                
                
                GetKeyFrame(MovementDistance: vector3) : []keyframe_delta=
                    MovementKeyFrame: []keyframe_delta = {
                        keyframe_delta
                        {
                            DeltaLocation := vector3 (-MovementDistance)
                            DeltaRotation := MakeRotationFromYawPitchRollDegrees(0.0, 0.0, 0.0)
                            DeltaScale := vector3 { X:=1.0, Y:=1.0, Z:=1.0 }
                            Time := 0.0
                            Interpolation := cubic_bezier_parameters {X0:= 0.0, Y0:=1.0, X1:= 0.5, Y1:= 0.9}
                        },
                        keyframe_delta
                        {
                            DeltaLocation := vector3 (MovementDistance)
                            DeltaRotation := MakeRotationFromYawPitchRollDegrees(0.0, 0.0, 0.0)
                            DeltaScale := vector3 { X:=1.0, Y:=1.0, Z:=1.0 }
                            Time := 2.0
                            Interpolation := cubic_bezier_parameters {X0:= 0.0, Y0:=1.0, X1:= 0.5, Y1:= 0.9}
                        }
                    }
                    return MovementKeyFrame
                
MyClassModule <public> := module:
    my_class <public>:= class:

        FooFunc<public>():void=
            return Print("[SUCCESS] FooFunc() | MyClassModule.my_class")
            
        LogicToString<public>(Logic:logic):string=
            ReturnedLogic:=if(Logic = true) then "[SUCCESS] false" else "[SUCCESS] true"
        

        Vector2ToString<public>(Vector2:vector2):string=
            return "[SUCCESS] [X = {Vector2.X}] [Y = {Vector2.Y}]"