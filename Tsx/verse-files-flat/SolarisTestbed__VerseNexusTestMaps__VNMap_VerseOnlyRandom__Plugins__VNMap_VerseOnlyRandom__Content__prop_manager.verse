
using { /Fortnite.com/Devices }
using { /Fortnite.com/Devices/CreativeAnimation }
using { /Verse.org/Native }
using { /Verse.org/Simulation }
using { /Verse.org/Random }
using { /UnrealEngine.com/Temporary/SpatialMath }

prop_manager := class(creative_device):
    @editable
    var ChairProp:creative_prop_asset := DefaultCreativePropAsset

    @editable
    var ChairProps:[]creative_prop_asset := array{}

    OnBegin<override>()<suspends>:void=
        Print ("Nexus OnBegin - prop_manager")
        Sleep(3.0)
        SpawnChairProps()

    OnEnd<override>():void=
        Print ("Nexus OnEnd - prop_manager")

    SpawnChairProps()<suspends>:void=
        SpawnPosition:vector3 := vector3{X:=-600.0, Y:=-1400.0, Z:= 0.0}
        TeleportPosition:vector3 := vector3{X:=4.0, Y:=-1400.0, Z:= 0.0}
        
        for (Prop : ChairProps):
            Print("Spawning prop...")
            SpawnedProp := SpawnProp(Prop, SpawnPosition, IdentityRotation())
            Sleep(1.0)

            if (CreativeProp := SpawnedProp(0)?):
                Print("Testing spawned prop...")

                if:
                    Controller := CreativeProp.GetAnimationController[]
                    false?
                then:
                    Print("Nexus Error - prop_manager.SpawnChairProps() | Unreachable")
                    Err("Unreachable")

                if:
                    CreativeProp.TeleportTo[TeleportPosition, IdentityRotation()]
                then:
                    Print("Woo hoo teleported!")

                    if:
                        Controller := CreativeProp.GetAnimationController[]
                    then:
                        Print("Setting an animation...")
                        Keyframes := array{keyframe_delta{DeltaLocation := vector3{X := 0.0, Y := 0.0, Z := 0.0}, DeltaRotation := IdentityRotation(), Time := 2.0}}
                        Controller.SetAnimation(Keyframes, ?Mode := animation_mode.OneShot)
                        Print("Playing the animation!")
                        Controller.Play()
                        Print("Awaiting next keyframe...")
                        Controller.AwaitNextKeyframe()
                        Print("Animation complete")

                Sleep(5.0)
                Print("Destroying spawned prop!")
                CreativeProp.Dispose()
