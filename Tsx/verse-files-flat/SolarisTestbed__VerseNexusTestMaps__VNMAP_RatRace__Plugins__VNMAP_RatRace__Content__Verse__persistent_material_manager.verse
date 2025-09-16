
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Colors }
using { /Verse.org/Random }

persistent_material_data := class<final><persistable>:
    ColorParam:color = color{}
    ScalarParam:float = 0.0

MakeMaterialData<constructor>(Color:color, Scalar:float)<transacts> := persistent_material_data:
    ColorParam := Color
    ScalarParam := Scalar

var MaterialDataMap:weak_map(player, persistent_material_data) = map{}

# A Verse-authored creative device that can be placed in a level
persistent_material_manager := class(creative_device):
    @editable
    TargetProp:creative_prop = creative_prop{}

    #ParamMaterial:Materials.PersistenceTestMaterial_material := Materials.PersistenceTestMaterial_material{}

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - persistent_material_manager")
        if:
            #set ParamMaterial.ColorParam = color{R:=1.0, G:=1.0, B:=1.0}
            #set ParamMaterial.ScalarParam = 1.0
            true = true
            
        Players := GetPlayspace().GetPlayers()
        for(Player:Players):
            if:
                not MaterialDataMap[Player]

                RandomColor:color = color{R:=GetRandomFloat(0.0,1.0),G:=GetRandomFloat(0.0,1.0),B:=GetRandomFloat(0.0,1.0)}
                RandomScalar:float = GetRandomFloat(0.0,1.0)
                NewMaterialData := MakeMaterialData(RandomColor, RandomScalar)

                set MaterialDataMap[Player] = NewMaterialData
                Print("persistent_material_manager: NewMaterialData created!")

            if:
                not MaterialDataMap[Player]

                RandomColor:color = color{R:=GetRandomFloat(0.0,1.0),G:=GetRandomFloat(0.0,1.0),B:=GetRandomFloat(0.0,1.0)}
                RandomScalar:float = GetRandomFloat(0.0,1.0)
                NewMaterialData := MakeMaterialData(RandomColor, RandomScalar)

                set MaterialDataMap[Player] = NewMaterialData

                true = false

                Print("NexusError - Reached end of an if statement that should always fail in persistent_material_manager")
        
            #if:
                #set ParamMaterial.ColorParam = MaterialDataMap[Player].ColorParam
                #set ParamMaterial.ScalarParam = MaterialDataMap[Player].ScalarParam
                #TargetProp.SetMaterial(ParamMaterial)

    OnEnd<override>():void=
        Print("Nexus OnEnd - persistent_material_manager")