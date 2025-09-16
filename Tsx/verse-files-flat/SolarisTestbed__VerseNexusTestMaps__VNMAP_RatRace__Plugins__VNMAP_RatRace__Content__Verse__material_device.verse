using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using {/Verse.org/Assets}
using {/Verse.org/Colors}
using {/Verse.org/Random}

# Test Asset Reflection for materials
# Things to try:
# Can changes be rolled back?
# Any issues changing rapidly between materials?
# Lerping between material colors

rat_player_data := struct:
    RatProp:creative_prop = creative_prop{}
    RatInt:int = 0


material_device := class(creative_device):
    @editable
    TargetProp:creative_prop = creative_prop{}
    @editable
    ParameterProp:creative_prop = creative_prop{}
    @editable
    SetMaterialLoopIterations:int = 200
    #ParamMaterial:Materials.Material4_material = Materials.Material4_material{}

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - material_device")
        Sleep(0.5)
        # Testing SetMaterial in an async code block
        branch:
            for(X := 0..SetMaterialLoopIterations):
                CycleMaterials()
        for(X := 0..SetMaterialLoopIterations):
            CycleMaterials()
        Print("Finished looping through materials {SetMaterialLoopIterations} times. Prop should be green.")
        Print("Sleep for 1.0 then apply paramaterized material with random color to the bed")
        Sleep(1.0)
        for(X := 0..SetMaterialLoopIterations):
            #TestParameters()
        

    # Cycle through setting all materials
    # This will be so fast you never see any materials but the last one, unless you add sleeps
    CycleMaterials()<suspends>:void=
        TargetProp.SetMaterial(Materials.Material1{})
        TargetProp.SetMaterial(Materials.Material2{})
        TargetProp.SetMaterial(Materials.Material3{})
    
    #TestParameters():void=
        #ParameterProp.SetMaterial(ParamMaterial)
        #set ParamMaterial.VectorParam = color{R:=GetRandomFloat(0.0,1.0),G:=GetRandomFloat(0.0,1.0),B:=GetRandomFloat(0.0,1.0)}
        #set ParamMaterial.ScalarParam = 1.0

    OnEnd<override>():void=
        Print("Nexus OnEnd - material_device")