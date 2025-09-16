
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /Verse.org/Simulation/Tags }
using { /Verse.org/Random }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }


props_tag := class(tag){}

props_random_device := class(creative_device):

    var Props:[]creative_object_interface := array{}
    var Props1:[]creative_prop := array{}
    PropTag:props_tag := props_tag{}
    var PropCount:int = 1

    EnvironmentRandomising():void=
        set Props = GetCreativeObjectsWithTag(PropTag)
        if:
            for(Prop:Props):
                set Props1 += array{creative_prop[Prop]}
        for(Prop:Props1):
            RandomVector:vector3 := vector3{X:= GetRandomFloat(-8386.338723, 18661.217547), Y:= GetRandomFloat(-2620.07, 22245.921), Z:= Prop.GetTransform().Translation.Z}
            set PropCount = PropCount + 1
            if:
                Prop.TeleportTo[RandomVector, Prop.GetTransform().Rotation]