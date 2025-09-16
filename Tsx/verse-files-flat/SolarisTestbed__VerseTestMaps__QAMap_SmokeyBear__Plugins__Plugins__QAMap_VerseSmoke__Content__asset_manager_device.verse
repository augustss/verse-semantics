using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation/Tags }
using { /Fortnite.com/Characters }
using { /Verse.org/Random }
using { /Verse.org/Colors }

asset_manager_device:= class (creative_device):
    @editable
    PropToChange : creative_prop = creative_prop {}

    @editable
    PropToSpawn : creative_prop_asset =DefaultCreativePropAsset

    @editable
    BillboardToText : billboard_device = billboard_device {}

    MessageToString<localizes>(Msg:string) : message = "{Msg}"

    @editable
    var IsMeshSet : logic = false

    @editable
    var IsMaterialSet : logic = false

    var SomeNewProps : []?creative_prop = array {}

    @editable
    var SpawnPropPosition : vector3 = vector3{X:=0.0,Y:=0.0,Z:=0.0}

    @editable
    TestProps:[]creative_prop := array{}

    <# TestMaterial:Materials.Mat_With_Params_material := Materials.Mat_With_Params_material{}
    SecondTestMaterial:Materials.Second_Mat_With_Params_material := Materials.Second_Mat_With_Params_material{} #>

    OnEnd<override>():void=
        Print("OnEnd asset_manager_device")
        
    OnBegin<override>()<suspends>:void=
        Print("OnBegin asset_manager_device")
        # StartingParametrizeMaterialsTestFunc()
        SetTheMeshes()
        SetTheMats()

        spawn:
            SpawnThePropsAndDispose()
    
    # Tests IsValid and SpawnProp[] 
    SpawnThePropsAndDispose()<suspends>:void=
        ARotation:= MakeRotation(vector3{X := 0.0, Y := 0.0, Z := 100.0}, 0.0)
        Scale:= vector3{X:=0.25,Y:=0.25, Z:= 0.25}
        for (Index:=0..0):
            set SomeNewProps += array{SpawnProp(PropToSpawn,Transform:= transform{Scale:= Scale, Rotation:= ARotation, Translation:= SpawnPropPosition})(0)}
            Print("[SUCCESS] Successfully added Props to our array and spawned them | SpawnThePropsAndDispose()", ?Color:=NamedColors.LightGreen)
        for(NewProp: SomeNewProps, ActualProp:=NewProp?):
            if (ActualProp.IsValid[]):
              ### ActualProp.Dispose() ###
            else:
                return

        Sleep(10.0)
            
        for(NewProp: SomeNewProps, ActualProp:=NewProp?):
            if (ActualProp.IsValid[]):
                Print ("[FAILURE] it's still valid, was the asset destroyed? | SpawnThePropsAndDispose()", ?Color:=NamedColors.Red)
                NotValidString: string = "The asset is still valid, if it's NOT there, this is a BUG. If it is there then you did not destroy it in time | SpawnThePropsAndDispose()" 
                BillboardToText.SetText(MessageToString(NotValidString))

            else:
                NotValidString: string = "The asset is no longer valid, it's either been disposed or destroyed | SpawnThePropsAndDispose()" 
                Print("[SUCCESS] it's not valid | SpawnThePropsAndDispose()", ?Color:=NamedColors.LightGreen)
                BillboardToText.SetText(MessageToString(NotValidString))
                
    # Tests AssetsReflection SetMesh()
    SetMesh()<suspends>:void=
        loop:
            Sleep (3.0)
            if (not IsMeshSet?):
                PropToChange.SetMesh(Meshes.Prop_Box)
                set IsMeshSet = true
                Print ("[SUCCESS] Setting a new Mesh | SetMesh()", ?Color:=NamedColors.LightGreen)
            else:
                PropToChange.SetMesh(Meshes.Prop_Sphere)
                set IsMeshSet = false
                Print ("[SUCCESS] Setting an old Mesh | SetMesh()", ?Color:=NamedColors.LightGreen)
                    
    SetTheMeshes():void=
        spawn:
            SetMesh()
                

    # Tests AssetReflection SetMaterial()
    SetMat()<suspends>:void=
        loop:
            Sleep(3.0)
            if (not IsMaterialSet?):
                PropToChange.SetMaterial(Materials.Mat_Blue)
                set IsMaterialSet = true
                Print ("[SUCCESS] Setting a new Material | SetMat()", ?Color:=NamedColors.LightGreen)
            else:
                PropToChange.SetMaterial(Materials.Mat_Red)
                set IsMaterialSet = false
                Print ("[SUCCESS] Setting a old Material | SetMat()", ?Color:=NamedColors.LightGreen)


    SetTheMats():void=
        spawn:
            SetMat()

    # Tests AssetReflection Parametrize Materials
    
    ## UtilsFunc
    <# StartingParametrizeMaterialsTestFunc():void=
        BeforeTestingSetup()
        spawn:
            TestStarter()

    ## Testing setup
    BeforeTestingSetup():void=
        for(TestProp:TestProps):
            TestProp.SetMaterial(TestMaterial)
        Print("The material was set on a props, | BeforeTestingSetup()", ?Color := NamedColors.LightGreen)

    TestStarter()<suspends>:void=
        SetSingleScalarParameterTest()
        SetMultipleScalarParametersTest()
        SetSingleVectorParameterTest()
        SetMultipleVectorParametersTest()
        SetSingleScalarAndSingleVectorParameters()
        SetMultipleScalarAndMultipleVectorParameters()
        SetMultipleScalarAndMultipleVectorParametersWithMaterialSwitching()

    ## Tests
    SetSingleScalarParameterTest()<suspends>:void=
        ExpectedResult := GetRandomFloat(0.0, 1.0)
        
        set TestMaterial.TestMetallic = ExpectedResult
        
        if(TestMaterial.TestMetallic = ExpectedResult):
            Print ("[SUCCESS] The material single scalar parameter was successfully changed  | SetSingleScalarParameterTest()", ?Color := NamedColors.LightGreen)
        else:
            Print ("[FAILURE] The material single scalar parameter was not successfully changed  | SetSingleScalarParameterTest()", ?Color := NamedColors.Red)

        Sleep(1.0)

    SetMultipleScalarParametersTest()<suspends>:void=
        FirstExpectedResult := GetRandomFloat(0.0, 1.0)
        SecondExpextedResult := GetRandomFloat(0.0, 1.0)
       
        set TestMaterial.TestMetallic = FirstExpectedResult
        set TestMaterial.TestSpecular = SecondExpextedResult
        
        if(TestMaterial.TestMetallic = FirstExpectedResult and TestMaterial.TestSpecular = SecondExpextedResult):
            Print ("[SUCCESS] The material multiple scalar parameters was successfully changed  | SetMultipleScalarParametersTest()", ?Color := NamedColors.LightGreen)
        else:
            Print ("[FAILURE] The material multiple scalar parameters was not successfully changed  | SetMultipleScalarParametersTest()", ?Color := NamedColors.Red)

        Sleep(1.0)

    SetSingleVectorParameterTest()<suspends>:void=
        ExpectedResult := color{R:=GetRandomFloat(0.0,1.0),G:=GetRandomFloat(0.0,1.0),B:=GetRandomFloat(0.0,1.0)}
        
        set TestMaterial.TestColor = ExpectedResult
        
        if(TestMaterial.TestColor.R = ExpectedResult.R and TestMaterial.TestColor.G = ExpectedResult.G and TestMaterial.TestColor.B = ExpectedResult.B):
            Print ("[SUCCESS] The material single vector parameter was successfully changed  | SetSingleVectorParameterTest()", ?Color := NamedColors.LightGreen)
        else:
            Print ("[FAILURE] The material single vector parameter was not successfully changed  | SetSingleVectorParameterTest()", ?Color := NamedColors.Red)

        Sleep(1.0)

    SetMultipleVectorParametersTest()<suspends>:void=
        FirstExpectedResult := color{R:=GetRandomFloat(0.0,1.0),G:=GetRandomFloat(0.0,1.0),B:=GetRandomFloat(0.0,1.0)}
        SecondExpextedResult := color{R:=GetRandomFloat(0.0,1.0),G:=GetRandomFloat(0.0,1.0),B:=GetRandomFloat(0.0,1.0)}

        set TestMaterial.TestColor = FirstExpectedResult
        set TestMaterial.TestEmissiveColor = SecondExpextedResult
        
        if(TestMaterial.TestColor.R = FirstExpectedResult.R and TestMaterial.TestColor.G = FirstExpectedResult.G and TestMaterial.TestColor.B = FirstExpectedResult.B and 
        TestMaterial.TestEmissiveColor.R = SecondExpextedResult.R and TestMaterial.TestEmissiveColor.G = SecondExpextedResult.G and TestMaterial.TestEmissiveColor.B = SecondExpextedResult.B):
            Print ("[SUCCESS] The material multiple vector parameters was successfully changed  | SetMultipleVectorParametersTest()", ?Color := NamedColors.LightGreen)
        else:
            Print ("[FAILURE] The material multiple vector parameters was not successfully changed  | SetMultipleVectorParametersTest()", ?Color := NamedColors.Red)

        Sleep(1.0)

    SetSingleScalarAndSingleVectorParameters()<suspends>:void=
        ScalarExpectedResult := GetRandomFloat(0.0, 1.0)
        VectorExpextedResult := color{R:=GetRandomFloat(0.0,1.0),G:=GetRandomFloat(0.0,1.0),B:=GetRandomFloat(0.0,1.0)}

        set TestMaterial.TestMetallic = ScalarExpectedResult
        set TestMaterial.TestColor = VectorExpextedResult

        if(TestMaterial.TestColor.R = VectorExpextedResult.R and TestMaterial.TestColor.G = VectorExpextedResult.G and 
        TestMaterial.TestColor.B = VectorExpextedResult.B and TestMaterial.TestMetallic = ScalarExpectedResult):
            Print ("[SUCCESS] The material single scalar and single vector parameters was successfully changed  | SetSingleScalarAndSingleVectorParameters()", ?Color := NamedColors.LightGreen)
        else:
            Print ("[FAILURE] The material single scalar and single vector parameters was successfully changed  | SetSingleScalarAndSingleVectorParameters()", ?Color := NamedColors.Red)

        Sleep(1.0)

    SetMultipleScalarAndMultipleVectorParameters()<suspends>:void=
        FirstScalarExpectedResult := GetRandomFloat(0.0, 1.0)
        SecondScalarExpectedResult := GetRandomFloat(0.0, 1.0)
        FirtsVectorExpextedResult := color{R:=GetRandomFloat(0.0,1.0),G:=GetRandomFloat(0.0,1.0),B:=GetRandomFloat(0.0,1.0)}
        SecondVectorExpextedResult := color{R:=GetRandomFloat(0.0,1.0),G:=GetRandomFloat(0.0,1.0),B:=GetRandomFloat(0.0,1.0)}

        set TestMaterial.TestMetallic = FirstScalarExpectedResult
        set TestMaterial.TestSpecular = SecondScalarExpectedResult
        set TestMaterial.TestColor = FirtsVectorExpextedResult
        set TestMaterial.TestEmissiveColor = SecondVectorExpextedResult

        if(TestMaterial.TestColor.R = FirtsVectorExpextedResult.R and TestMaterial.TestColor.G = FirtsVectorExpextedResult.G and 
        TestMaterial.TestColor.B = FirtsVectorExpextedResult.B and TestMaterial.TestMetallic = FirstScalarExpectedResult and
        TestMaterial.TestSpecular = SecondScalarExpectedResult and TestMaterial.TestEmissiveColor.R = SecondVectorExpextedResult.R and 
        TestMaterial.TestEmissiveColor.G = SecondVectorExpextedResult.G and TestMaterial.TestEmissiveColor.B = SecondVectorExpextedResult.B):
            Print ("[SUCCESS] The material multiple scalar and multiple vector parameters was successfully changed  | SetMultipleScalarAndMultipleVectorParameters()", ?Color := NamedColors.LightGreen)
        else:
            Print ("[FAILURE] The material multiple scalar and multiple vector parameters was successfully changed  | SetMultipleScalarAndMultipleVectorParameters()", ?Color := NamedColors.Red)

        Sleep(1.0)

    SetMultipleScalarAndMultipleVectorParametersWithMaterialSwitching()<suspends>:void=
        FirstScalarExpectedResult := GetRandomFloat(0.0, 1.0)
        SecondScalarExpectedResult := GetRandomFloat(0.0, 1.0)
        FirtsVectorExpextedResult := color{R:=GetRandomFloat(0.0,1.0),G:=GetRandomFloat(0.0,1.0),B:=GetRandomFloat(0.0,1.0)}
        SecondVectorExpextedResult := color{R:=GetRandomFloat(0.0,1.0),G:=GetRandomFloat(0.0,1.0),B:=GetRandomFloat(0.0,1.0)}

        for(TestProp:TestProps):
            TestProp.SetMaterial(SecondTestMaterial)

        set SecondTestMaterial.TestMetallic = FirstScalarExpectedResult

        if(SecondTestMaterial.TestMetallic = FirstScalarExpectedResult):
            Print ("[SUCCESS] The SecondTestMaterial single scalar parameter was successfully changed  | SetMultipleScalarAndMultipleVectorParametersWithMaterialSwitching()", ?Color := NamedColors.LightGreen)
        else:
            Print ("[FAILURE] The SecondTestMaterial single scalar parameter was not successfully changed  | SetMultipleScalarAndMultipleVectorParametersWithMaterialSwitching()", ?Color := NamedColors.Red)

        Sleep(1.0)

        for(TestProp:TestProps):
            TestProp.SetMaterial(TestMaterial)

        set TestMaterial.TestMetallic = SecondScalarExpectedResult

        if(TestMaterial.TestMetallic = SecondScalarExpectedResult):
            Print ("[SUCCESS] The TestMaterial single scalar parameter was successfully changed  | SetMultipleScalarAndMultipleVectorParametersWithMaterialSwitching()", ?Color := NamedColors.LightGreen)
        else:
            Print ("[FAILURE] The TestMaterial single scalar parameter was not successfully changed  | SetMultipleScalarAndMultipleVectorParametersWithMaterialSwitching()", ?Color := NamedColors.Red)

        Sleep(1.0)

        for(TestProp:TestProps):
            TestProp.SetMaterial(SecondTestMaterial)
        
        set SecondTestMaterial.TestColor = FirtsVectorExpextedResult

        if(SecondTestMaterial.TestColor.R = FirtsVectorExpextedResult.R and SecondTestMaterial.TestColor.G = FirtsVectorExpextedResult.G and 
        SecondTestMaterial.TestColor.B = FirtsVectorExpextedResult.B):
            Print ("[SUCCESS] The SecondTestMaterial single vector parameters was successfully changed  | SetMultipleScalarAndMultipleVectorParametersWithMaterialSwitching()", ?Color := NamedColors.LightGreen)
        else:
            Print ("[FAILURE] The SecondTestMaterial and single vector parameters was successfully changed  | SetMultipleScalarAndMultipleVectorParametersWithMaterialSwitching()", ?Color := NamedColors.Red)

        Sleep(1.0)

        for(TestProp:TestProps):
            TestProp.SetMaterial(TestMaterial)

        set TestMaterial.TestColor = SecondVectorExpextedResult

        if(TestMaterial.TestColor.R = SecondVectorExpextedResult.R and TestMaterial.TestColor.G = SecondVectorExpextedResult.G and 
        TestMaterial.TestColor.B = SecondVectorExpextedResult.B):

            Print ("[SUCCESS] The TestMaterial single scalar and single vector parameters was successfully changed  | SetMultipleScalarAndMultipleVectorParametersWithMaterialSwitching()", ?Color := NamedColors.LightGreen)
        else:
            Print ("[FAILURE] The TestMaterial single scalar and single vector parameters was successfully changed  | SetMultipleScalarAndMultipleVectorParametersWithMaterialSwitching()", ?Color := NamedColors.Red) #>
    