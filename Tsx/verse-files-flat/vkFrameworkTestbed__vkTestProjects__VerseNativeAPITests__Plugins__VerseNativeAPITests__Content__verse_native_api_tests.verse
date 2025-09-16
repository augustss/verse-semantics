using { /Fortnite.com/Devices }
using { /Fortnite.com/Playspaces }
using { /Fortnite.com/Game }
using { /Fortnite.com/UI }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/UI }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Colors }
using { /Verse.org/Random }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/FortPlayerUtilities }
using { /Fortnite.com/Characters }
using { /Fortnite.com/AI }
using { /Verse.org/Assets }
using { /Verse.org/Simulation/Tags }
using { /Fortnite.com/Vehicles }
using { /Fortnite.com/Devices/CreativeAnimation }
using { /UnrealEngine.com/Temporary/Curves }
using { /Verse.org/Concurrency }
 
text_test<public> := class<final>(text_base):

MyTag := class(tag){}

MyLogChannel := class(log_channel){}

SuspendsFunction()<suspends>:void=
    Print("SuspendsFunction")
    Sleep(2.0)

# Device that should call all public/native verse APIs
public_native_verse_apis_tests := class(creative_device):
 
    var MyPlayer:?player = false
 
    TestMessage<localizes> : message = "The Message"

    # Random Widget to use
    ButtonWidget : button_loud = button_loud{}

    SomeVec3 : vector3 = vector3{X:=1.0, Y:=1.0, Z:=1.0}
    SomeVec2 : vector2 = vector2{X:=1.0, Y:=1.0}
    SomeVec2i : vector2i = vector2i{X:=1, Y:=1}

    # Event setup
    MyEvent:event() := event(){}
    EventSignal()<suspends>:void={ MyEvent.Signal() }
    EventRun()<suspends>:void=
        Print("  MyEvent Run")
        race:
            MyEvent.Await()
            block:
                EventSignal()

    # UI::slider_regular<native><public> := class<final>(widget):
    # UI::slider_regular::SetValue<native><public>(InValue:float):void
    # UI::slider_regular::GetValue<native><public>():float
    # UI::slider_regular::SetMinValue<native><public>(InMinValue:float):void
    # UI::slider_regular::GetMinValue<native><public>():float
    # UI::slider_regular::SetMaxValue<native><public>(InMaxValue:float):void
    # UI::slider_regular::GetMaxValue<native><public>():float
    # UI::slider_regular::SetStepSize<native><public>(InValue:float):void
    # UI::slider_regular::GetStepSize<native><public>():float
    Silder():void=
        Print("Slider")
 
        MySlider := slider_regular{}
        MySlider.SetValue(0.0)
        MySlider.GetValue()
        MySlider.SetMinValue(0.0)
        MySlider.GetMinValue()
        MySlider.SetMaxValue(1.0)
        MySlider.GetMaxValue()
        MySlider.SetStepSize(0.1)
        MySlider.GetStepSize()

    # UI::text_block<native><public> := class<final>(text_base):
    # UI::text_block::SetShadowOffset<native><public>(InShadowOffset:?vector2):void
    # UI::text_block::GetShadowOffset<native><public>():?vector2
    # UI::text_block::SetShadowColor<native><public>(InColor:color):void
    # UI::text_block::GetShadowColor<native><public>():color
    # UI::text_block::SetShadowOpacity<native><public>(InOpacity:type {_X:float where 0.000000 <= _X, _X <= 1.000000}):void
    # UI::text_block::GetShadowOpacity<native><public>():type {_X:float where 0.000000 <= _X, _X <= 1.000000}
    TextBlock():void=
        Print("TextBlock")
 
        MyTextBlock := text_block{}
        MyTextBlock.SetShadowOffset(MyTextBlock.DefaultShadowOffset)
        MyTextBlock.GetShadowOffset()
        MyTextBlock.SetShadowColor(MyTextBlock.DefaultShadowColor)
        MyTextBlock.GetShadowColor()
        MyTextBlock.SetShadowOpacity(MyTextBlock.GetShadowOpacity())

    # UI::text_button_base<native><public> := class<abstract><epic_internal>(widget):
    # UI::text_button_base::SetText<native><public>(InText:message):void
    # UI::text_button_base::GetText<native><public>():[]char
    # UI::button_loud<native><public> := class<final>(text_button_base):
    # UI::button_regular<native><public> := class<final>(text_button_base):
    # UI::button_quiet<native><public> := class<final>(text_button_base):
    # Temporary::UI::button<native><public> := class<final>(widget):
    # Temporary::UI::button::SetWidget<native><public>(InSlot:button_slot):void
    Button():void=
        Print("Button")
 
        MyTextButton := button_regular{}
        MyTextButton.SetText(TestMessage)
        MyTextButton.GetText()

        MyLoudButton := button_loud{}
        MyQuietButton := button_quiet{}

        MyButtonSlot := button_slot{Widget:=MyLoudButton}
        MyOtherButtonSlot := button_slot{Widget:=MyQuietButton}
        MyButton := button{Slot:=MyButtonSlot}
        MyButton.SetWidget(MyOtherButtonSlot)

    # Temporary::UI::overlay<native><public> := class<final>(widget):
    # Temporary::UI::overlay::AddWidget<native><public>(Slot:overlay_slot):void
    # Temporary::UI::overlay::RemoveWidget<native><public>(Widget:widget):void
    Overlay():void=
        Print("Overlay")

        MyOverlay := overlay{}
        MyOverlaySlot := overlay_slot{Widget:=ButtonWidget}
        MyOverlay.AddWidget(MyOverlaySlot)
        MyOverlay.RemoveWidget(ButtonWidget)

    # Temporary::UI::stack_box<native><public> := class<final>(widget):
    # Temporary::UI::stack_box::AddWidget<native><public>(Slot:stack_box_slot):void
    # Temporary::UI::stack_box::RemoveWidget<native><public>(Widget:widget):void
    StackBox():void=
        Print("StackBox")

        MyStackBox := stack_box{Orientation:=orientation.Vertical}
        MyStackBoxSlot := stack_box_slot{Widget:=ButtonWidget}
        MyStackBox.AddWidget(MyStackBoxSlot)
        MyStackBox.RemoveWidget(ButtonWidget)

    # Temporary::UI::GetPlayerUI<native><public>(Player:player)<transacts><decides>:player_ui
    # Temporary::UI::player_ui::AddWidget<native><public>(Widget:widget):void
    # Temporary::UI::player_ui::AddWidget<native><public>(Widget:widget, Slot:player_ui_slot):void
    # Temporary::UI::player_ui::RemoveWidget<native><public>(Widget:widget):void
    # UI::(PlayerUI:player_ui).ShowHUDElements<native><public>(HUDElements:[]hud_element_identifier):void
    # UI::(PlayerUI:player_ui).HideHUDElements<native><public>(HUDElements:[]hud_element_identifier):void
    # UI::(PlayerUI:player_ui).ResetHUDElementVisibility<native><public>(HUDElements:[]hud_element_identifier):void
    PlayerUI():void=
        Print("PlayerUI")
 
        PlayerUISlot := player_ui_slot{}
 
        if (LocalPlayer := MyPlayer?):
            Print(" LocalPlayer Valid")
            if (MyPlayerUI := GetPlayerUI[LocalPlayer]):
                Print("   PlayerUI Valid")
                MyPlayerUI.AddWidget(ButtonWidget)
                MyPlayerUI.AddWidget(ButtonWidget, PlayerUISlot)
                MyPlayerUI.RemoveWidget(ButtonWidget)
                MyPlayerUI.ShowHUDElements()
                MyPlayerUI.HideHUDElements()
                MyPlayerUI.ResetHUDElementVisibility()
 
    # Temporary::UI::widget::SetEnabled<native><public>(InIsEnabled:logic):void
    # Temporary::UI::widget::IsEnabled<native><public>():logic
    # Temporary::UI::widget::GetParentWidget<native><public>()<transacts><decides>:widget
    # Temporary::UI::widget::GetRootWidget<native><public>()<transacts><decides>:widget
    # Temporary::UI::widget::SetVisibility<native><public>(InVisibility:widget_visibility):void
    # Temporary::UI::widget::GetVisibility<native><public>():widget_visibility
    Widget():void=
        Print("Widget")

        RandomWidget : button_loud = button_loud{}

        if (ParentWidget := RandomWidget.GetParentWidget[]):
            if (RootWidget := RandomWidget.GetRootWidget[]):
 
        ButtonWidget.SetVisibility(RandomWidget.GetVisibility())
        ButtonWidget.SetEnabled(RandomWidget.IsEnabled())

    # Temporary::UI::text_base<native><public> := class<abstract>(widget):
    # Temporary::UI::text_base::SetText<native><public>(InText:message):void
    # Temporary::UI::text_base::GetText<native><public>():[]char
    # Temporary::UI::text_base::SetJustification<native><public>(InJustification:text_justification):void
    # Temporary::UI::text_base::GetJustification<native><public>():text_justification
    # Temporary::UI::text_base::SetOverflowPolicy<native><public>(InOverflowPolicy:text_overflow_policy):void
    # Temporary::UI::text_base::GetOverflowPolicy<native><public>():text_overflow_policy
    # Temporary::UI::text_base::SetTextColor<native><public>(InColor:color):void
    # Temporary::UI::text_base::GetTextColor<native><public>():color
    # Temporary::UI::text_base::SetTextOpacity<native><public>(InOpacity:type {_X:float where 0.000000 <= _X, _X <= 1.000000}):void
    # Temporary::UI::text_base::GetTextOpacity<native><public>():type {_X:float where 0.000000 <= _X, _X <= 1.000000}
    TextBase():void=
        Print("TextBase")
 
        MyTestTextBase := text_test{}
        MyTestTextBase.SetText(TestMessage)
        MyTestTextBase.GetText()
        MyTestTextBase.SetJustification(MyTestTextBase.GetJustification())
        MyTestTextBase.SetOverflowPolicy(MyTestTextBase.GetOverflowPolicy())
        MyTestTextBase.SetTextColor(MyTestTextBase.GetTextColor())
        MyTestTextBase.SetTextOpacity(MyTestTextBase.GetTextOpacity())
   
    # Colors::operator'+'<native><public>(c0:color, c1:color)<converges>:color
    # Colors::operator'-'<native><public>(c0:color, c1:color)<converges>:color
    # Colors::operator'*'<native><public>(c0:color, c1:color)<converges>:color
    # Colors::operator'*'<native><public>(c:color, factor:float)<converges>:color
    # Colors::operator'*'<native><public>(c:color, factor:int)<converges>:color
    # Colors::operator'*'<native><public>(factor:float, c:color)<converges>:color
    # Colors::operator'*'<native><public>(factor:int, c:color)<converges>:color
    # Colors::operator'/'<native><public>(c:color, factor:float)<computes><decides>:color
    # Colors::operator'/'<native><public>(c:color, factor:int)<computes><decides>:color
    # Colors::MakeColorFromSRGB<native><public>(Red:float, Green:float, Blue:float)<converges>:color
    # Colors::MakeSRGBFromColor<native><public>(InColor:color)<converges>:tuple(float, float, float)
    # Colors::MakeColorFromSRGBValues<native><public>(Red:int, Green:int, Blue:int)<converges>:color
    # Colors::MakeColorFromHex<native><public>(hexString:[]char)<converges>:color
    # Colors::MakeColorFromHSV<native><public>(Hue:float, Saturation:float, Value:float)<converges>:color
    # Colors::MakeHSVFromColor<native><public>(InColor:color):tuple(float, float, float)
    # Colors::MakeColorFromTemperature<native><public>(Temperature:float)<converges>:color
    Colors():void=
        Print("Colors")
 
        MyRedColor := color{R := 1.0, G := 0.0, B:= 0.0}
        MyBlueColor := color{R := 0.0, G := 0.0, B:= 1.0}
 
        AddColor        := MyRedColor + MyBlueColor
        SubColor        := MyRedColor - MyBlueColor
        MulColor        := MyRedColor * MyBlueColor
        MulByFloat      := MyRedColor * 1.0
        MulByInt        := MyRedColor * 1
        MulFloatFirst   := 1.0 * MyBlueColor
        MulIntFirst     := 1 * MyBlueColor
        if (DivByFloat := MyRedColor / 1.0):
        if (DivByInt := MyRedColor / 1):
 
        SomeColorFromFloat := MakeColorFromSRGB(1.0, 1.0, 1.0)
        SRBGFromColor := MakeSRGBFromColor(SomeColorFromFloat)
        SomeColorFromInt := MakeColorFromSRGBValues(1, 1, 1)
        ColorFromHex := MakeColorFromHex("#ff0000")
        ColorFromHsv := MakeColorFromHSV(1.0, 1.0, 1.0)
        HsvFromColor := MakeHSVFromColor(ColorFromHsv)
        ColorTemp := MakeColorFromTemperature(1.0)

    # Temporary::UI::color_block<native><public> := class<final>(widget):
    # Temporary::UI::color_block::SetColor<native><public>(InColor:color):void
    # Temporary::UI::color_block::GetColor<native><public>():color
    # Temporary::UI::color_block::SetOpacity<native><public>(InOpacity:type {_X:float where 0.000000 <= _X, _X <= 1.000000}):void
    # Temporary::UI::color_block::GetOpacity<native><public>():type {_X:float where 0.000000 <= _X, _X <= 1.000000}
    # Temporary::UI::color_block::SetDesiredSize<native><public>(InDesiredSize:vector2):void
    # Temporary::UI::color_block::GetDesiredSize<native><public>():vector2
    ColorBlock():void=
        Print("ColorBlock")
 
        MyColorBlock := color_block{}
 
        MyColorBlock.SetColor(MyColorBlock.GetColor())
        MyColorBlock.SetOpacity(MyColorBlock.GetOpacity())
        MyColorBlock.SetDesiredSize(MyColorBlock.GetDesiredSize())

    # Temporary::UI::canvas<native><public> := class<final>(widget):
    # Temporary::UI::canvas::AddWidget<native><public>(Slot:canvas_slot):void
    # Temporary::UI::canvas::RemoveWidget<native><public>(Widget:widget):void
    # Temporary::UI::MakeCanvasSlot<native><public>(Widget:widget, Position:vector2, ?Size:vector2 = external {}, ?ZOrder:type {_X:int where 0 <= _X, _X <= 2147483647} = external {}, ?Alignment:vector2 = external {})<computes>:canvas_slot
    Canvas():void=
        Print("Canvas")
 
        MyCanvasSlot := MakeCanvasSlot(ButtonWidget, vector2{X:=0.0, Y:=0.0})
        MyCanvasSlots : []canvas_slot = array{ MakeCanvasSlot(ButtonWidget, vector2{X:=0.0, Y:=0.0}) }
        MyCanvas := canvas{Slots := MyCanvasSlots}

        MyCanvas.AddWidget(MyCanvasSlot)
        MyCanvas.RemoveWidget(ButtonWidget)
   
    # Devices::creative_device<native><public> := class<concrete>(creative_object_interface):
    # Devices::creative_device::Show<native><public>()<transacts>:void
    # Devices::creative_device::Hide<native><public>()<transacts>:void
    # Devices::creative_device_base<native><public> := class<abstract><epic_internal>(creative_object):
    CreativeDevice():void=
        Print("CreativeDevice")
 
        MyDevice := creative_device{}
        MyDevice.Show()
        MyDevice.Hide()
   
    # Devices::creative_prop<native><public> := class<concrete><final>(creative_object, invalidatable):
    # Devices::creative_prop::Dispose<native><override>():void
    # Devices::creative_prop::IsValid<native><override>()<transacts><decides>:void
    # Devices::creative_prop::SetMesh<native><public>(Mesh:mesh)<transacts>:void
    # Devices::creative_prop::SetMaterial<native><public>(Material:material, ?Index:int = external {})<transacts>:void
    # Devices::creative_prop::Show<native><public>()<transacts>:void
    # Devices::creative_prop::Hide<native><public>()<transacts>:void
    CreativeProp():void=
        Print("CreativeProp")

        MyRotation := MakeRotation(SomeVec3, 1.0)
        MySpawnedProp := SpawnProp(DefaultCreativePropAsset, SomeVec3, MyRotation)

        if (MyProp := MySpawnedProp(0)?):
            if (MyProp.IsValid[]):
                Print("  CreativeProp IsValid")
                MyProp.SetMesh(Meshes.VerseDeviceStarter_ScifiBuilding_01)
                MyProp.SetMaterial(Materials.M_LightBeacon)
                MyProp.Show()
                MyProp.Hide()
                MyProp.Dispose()

    # Devices::GetCreativeObjectsWithTag<native><public>(Tag:tag)<transacts>:[]creative_object_interface
    # Devices::GetCreativeObjectsWithTags<native><public>(SearchCriteria:tag_search_criteria)<transacts>:[]creative_object_interface
    # Devices::(CreativeObject:creative_object_interface).GetTags<native><public>():tag_view
    # Devices::(CreativeObject:creative_object_interface).GetPlayspace<native><public>()<transacts>:fort_playspace
    CreativeObjectAndTags():void=
        Print("CreativeObject/Tags")

        MyTagSearch := tag_search_criteria{}

        GetCreativeObjectsWithTag(MyTag{})
        GetCreativeObjectsWithTags(MyTagSearch)

        MyTagView := Self.GetTags()

    # Verse::Ceil<native><public>(Val:float)<varies><decides>:int
    # Verse::Floor<native><public>(Val:float)<varies><decides>:int
    # Verse::Round<native><public>(Val:float)<varies><decides>:int
    # Verse::Int<native><public>(Val:float)<varies><decides>:int
    # Verse::Clamp<native><public>(Val:int, A:int, B:int)<computes>:int
    # Verse::Sqrt<native><public>(X:float)<varies>:float
    # Verse::Sin<native><public>(X:float)<varies>:float
    # Verse::Cos<native><public>(X:float)<varies>:float
    # Verse::Tan<native><public>(X:float)<varies>:float
    # Verse::ArcSin<native><public>(X:float)<varies>:float
    # Verse::ArcCos<native><public>(X:float)<varies>:float
    # Verse::ArcTan<native><public>(X:float)<varies>:float
    # Verse::ArcTan<native><public>(Y:float, X:float)<varies>:float
    # Verse::Sinh<native><public>(X:float)<varies>:float
    # Verse::Cosh<native><public>(X:float)<varies>:float
    # Verse::Tanh<native><public>(X:float)<varies>:float
    # Verse::ArSinh<native><public>(X:float)<varies>:float
    # Verse::ArCosh<native><public>(X:float)<varies>:float
    # Verse::ArTanh<native><public>(X:float)<varies>:float
    # Verse::Pow<native><public>(A:float, B:float)<varies>:float
    # Verse::Quotient<native><public>(X:int, Y:int)<computes><decides>:int
    # Verse::Mod<native><public>(X:int, Y:int)<computes><decides>:int
    # Verse::Exp<native><public>(X:float)<varies>:float
    # Verse::Ln<native><public>(X:float)<varies>:float
    # Verse::Lerp<native><public>(From:float, To:float, Parameter:float)<varies>:float
    Math():void=
        Print("Math")

        if (Ceil[1.0]):
        if (Floor[1.0]):
        if (Round[0.5]):
        if (Int[5.5]):

        Clamp(5, 0, 10)
        Sqrt(1.0)
        Sin(1.0)
        Cos(1.0)
        Tan(1.0)
        ArcSin(1.0)
        ArcCos(1.0)
        ArcTan(1.0)
        ArcTan(1.0, 1.0)
        Sinh(1.0)
        Cosh(1.0)
        Tanh(1.0)
        ArSinh(1.0)
        ArCosh(1.0)
        ArTanh(1.0)
        Pow(1.0, 1.0)
        if (Quotient[1, 1]):
        if (Mod[1, 10]):
        Exp(1.0)
        Ln(1.0)
        Lerp(0.0, 1.0, 0.5)

    # Random::GetRandomFloat<native><public>(Low:float, High:float)<varies>:float
    # Random::GetRandomInt<native><public>(Low:int, High:int)<varies>:int
    Random():void=
        Print("Random")

        RandomFloat := GetRandomFloat(0.0, 1.0)
        RandomInt := GetRandomInt(0, 10)

    # Verse::ToString<native><public>(Val:float)<varies>:[]char
    # Verse::ToString<native><public>(Val:int)<computes>:[]char  
    # Verse::ToString<native><public>(Character:char)<computes>:[]char
    # Temporary::SpatialMath::ToString<native><public>(V:vector3)<varies>:[]char
    # Temporary::SpatialMath::ToString<native><public>(V:vector2)<varies>:[]char
    # Temporary::SpatialMath::ToString<native><public>(V:vector2i)<computes>:[]char
    # Temporary::SpatialMath::ToString<native><public>(Rotation:rotation)<varies>:[]char
    ToStringTest():void=
        Print("ToString")

        FloatToString := ToString(1.0)
        IntToString := ToString(1)
        CharToString := ToString('c')

        Vec3ToString := ToString(SomeVec3)
        Vec2ToString := ToString(SomeVec2)
        Vec2iToString := ToString(SomeVec2i)

        SomeRotation := MakeRotation(SomeVec3, 1.0)
        RotationToString := ToString(SomeRotation)

    # Temporary::SpatialMath::MakeRotation<native><public>(Axis:vector3, AngleRadians:float)<varies>:rotation
    # Temporary::SpatialMath::MakeRotationFromYawPitchRollDegrees<native><public>(YawRightDegrees:float, PitchUpDegrees:float, RollClockwiseDegrees:float)<varies>:rotation
    # Temporary::SpatialMath::IdentityRotation<native><public>()<converges>:rotation
    # Temporary::SpatialMath::Distance<native><public>(Rotation1:rotation, Rotation2:rotation)<varies>:float
    # Temporary::SpatialMath::AngularDistance<native><public>(Rotation1:rotation, Rotation2:rotation)<varies>:float
    # Temporary::SpatialMath::(InitialRotation:rotation).ApplyPitch<native><public>(PitchUpRadians:float)<transacts>:rotation
    # Temporary::SpatialMath::(InitialRotation:rotation).ApplyRoll<native><public>(RollClockwiseRadians:float)<transacts>:rotation
    # Temporary::SpatialMath::(InitialRotation:rotation).ApplyYaw<native><public>(YawRightRadians:float)<transacts>:rotation
    # Temporary::SpatialMath::(InitialRotation:rotation).ApplyWorldRotationX<native><public>(AngleRadians:float)<transacts>:rotation
    # Temporary::SpatialMath::(InitialRotation:rotation).ApplyWorldRotationY<native><public>(AngleRadians:float)<transacts>:rotation
    # Temporary::SpatialMath::(InitialRotation:rotation).ApplyWorldRotationZ<native><public>(AngleRadians:float)<transacts>:rotation
    # Temporary::SpatialMath::(InitialRotation:rotation).RotateBy<native><public>(AdditionalRotation:rotation)<transacts>:rotation
    # Temporary::SpatialMath::(InitialRotation:rotation).UnrotateBy<native><public>(RotationToRemove:rotation)<transacts>:rotation
    # Temporary::SpatialMath::(Rotation:rotation).GetYawPitchRollDegrees<native><public>()<varies>:[]float
    # Temporary::SpatialMath::(Rotation:rotation).GetAxis<native><public>()<varies>:vector3
    # Temporary::SpatialMath::(Rotation:rotation).GetAngle<native><public>()<varies>:float
    # Temporary::SpatialMath::MakeShortestRotationBetween<native><public>(InitialRotation:rotation, FinalRotation:rotation)<transacts>:rotation
    # Temporary::SpatialMath::MakeShortestRotationBetween<native><public>(InitialVector:vector3, FinalVector:vector3)<transacts>:rotation
    # Temporary::SpatialMath::Slerp<native><public>(InitialRotation:rotation, FinalRotation:rotation, Parameter:float)<transacts><decides>:rotation
    # Temporary::SpatialMath::(Rotation:rotation).RotateVector<native><public>(Vector:vector3)<transacts>:vector3
    # Temporary::SpatialMath::(Rotation:rotation).UnrotateVector<native><public>(Vector:vector3)<transacts>:vector3
    # Temporary::SpatialMath::(Rotation:rotation).Invert<native><public>()<transacts>:rotation
    # Temporary::SpatialMath::(Rotation:rotation).IsFinite<native><public>()<computes><decides>:rotation
    # Temporary::SpatialMath::TransformVector<native><public>(InTransform:transform, InVector:vector3)<varies>:vector3
    # Temporary::SpatialMath::TransformVectorNoScale<native><public>(InTransform:transform, InVector:vector3)<varies>:vector3
    # Temporary::SpatialMath::ReflectVector<native><public>(Direction:vector2, SurfaceNormal:vector2)<varies><decides>:vector2
    # Temporary::SpatialMath::DotProduct<native><public>(V1:vector2, V2:vector2)<varies>:float
    # Temporary::SpatialMath::Distance<native><public>(V1:vector2, V2:vector2)<varies>:float
    # Temporary::SpatialMath::DistanceSquared<native><public>(V1:vector2, V2:vector2)<varies>:float
    # Temporary::SpatialMath::(V:vector2).MakeUnitVector<native><public>()<varies><decides>:vector2
    # Temporary::SpatialMath::ReflectVector<native><public>(Direction:vector3, SurfaceNormal:vector3)<varies><decides>:vector3
    # Temporary::SpatialMath::DotProduct<native><public>(V1:vector3, V2:vector3)<varies>:float
    # Temporary::SpatialMath::CrossProduct<native><public>(V1:vector3, V2:vector3)<varies>:vector3
    # Temporary::SpatialMath::Distance<native><public>(V1:vector3, V2:vector3)<varies>:float
    # Temporary::SpatialMath::DistanceSquared<native><public>(V1:vector3, V2:vector3)<varies>:float
    # Temporary::SpatialMath::DistanceXY<native><public>(V1:vector3, V2:vector3)<varies>:float
    # Temporary::SpatialMath::DistanceSquaredXY<native><public>(V1:vector3, V2:vector3)<varies>:float
    # Temporary::SpatialMath::(V:vector3).MakeUnitVector<native><public>()<varies><decides>:vector3
    SpatialMath():void=
        Print("SpatialMath")

        SomeRotation := MakeRotation(SomeVec3, 1.0)
        MyRotation := MakeRotationFromYawPitchRollDegrees(1.0, 1.0, 1.0)
        IdentRot := IdentityRotation()
        Dist := Distance(MyRotation, IdentRot)
        AngDist := AngularDistance(MyRotation, IdentRot)

        MyRotation.ApplyPitch(1.0)
        MyRotation.ApplyRoll(1.0)
        MyRotation.ApplyYaw(1.0)
        MyRotation.ApplyWorldRotationX(1.0)
        MyRotation.ApplyWorldRotationY(1.0)
        MyRotation.ApplyWorldRotationZ(1.0)
        MyRotation.RotateBy(IdentRot)
        MyRotation.UnrotateBy(IdentRot)

        MyRotation.RotateVector(SomeVec3)
        MyRotation.UnrotateVector(SomeVec3)

        MyRotation.Invert()
        if (MyRotation.IsFinite[]):

        RawPitchRollDegree := MyRotation.GetYawPitchRollDegrees()
        Axis := MyRotation.GetAxis()
        Angle := MyRotation.GetAngle()

        MakeShortestRotationBetween(MyRotation, IdentRot)
        MakeShortestRotationBetween(SomeVec3, SomeVec3)
        if (Slerp[MyRotation, IdentRot, 1.0]):

        SomeTransform := transform{Scale:=SomeVec3, Rotation:=MyRotation, Translation:=SomeVec3}

        TransformVector(SomeTransform, SomeVec3)
        TransformVectorNoScale(SomeTransform, SomeVec3)
        DotProduct(SomeVec2, SomeVec2)
        DotProduct(SomeVec3, SomeVec3)
        Distance(SomeVec2, SomeVec2)
        Distance(SomeVec3, SomeVec3)
        DistanceSquared(SomeVec2, SomeVec2)
        DistanceSquared(SomeVec3, SomeVec3)
        CrossProduct(SomeVec3, SomeVec3)
        DistanceXY(SomeVec3, SomeVec3)

        if (SomeVec2.MakeUnitVector[]):
        if (SomeVec3.MakeUnitVector[]):
        if (ReflectVector[SomeVec2, SomeVec2]):
        if (ReflectVector[SomeVec3, SomeVec3]):

    # Verse::Localize<native><public>(Message:message)<varies>:[]char
    LocalizeTest():void=
        Print("Localize")

        Localize(TestMessage)

    # Verse::invalidatable<native><public> := interface(disposable):
    InvalidatableTest():void=
        Print("Invalidatable")

        # is this right?
        Invalidatable := invalidatable

    # Verse::Join<native><public>(Strings:[][]char, Separator:[]char)<computes>:[]char
    StringTest():void=
        Print("String")

        DoubleArray := {"this", "is", "test"}
        Join(DoubleArray, "+")

    # FortPlayerUtilities::(InPlayer:player).SendToLobby<native><public>()<transacts>:void
    # FortPlayerUtilities::(InPlayer:player).IsSpectator<native><public>()<transacts><decides>:void
    # FortPlayerUtilities::(InPlayer:player).GetPlayersSpectating<native><public>()<transacts>:[]player
    # FortPlayerUtilities::(InAgent:agent).Respawn<native><public>(RespawnPosition:vector3, RespawnRotation:rotation)<transacts>:void
    # Characters::(InAgent:agent).GetFortCharacter<native><public>()<transacts><decides>:fort_character
    # Characters::(InAgent:agent).GetInstigator<native><public>()<transacts>:game_action_instigator
    # Characters::(InInstigator:game_action_instigator).GetInstigatorAgent<native><public>()<transacts><decides>:agent
    # AI::(InCharacter:fort_character).GetFocusInterface<native><public>()<transacts><decides>:focus_interface
    # AI::(InCharacter:fort_character).GetFortLeashable<native><public>()<transacts><decides>:fort_leashable
    # AI::MakeNavigationTarget<constructor><native><public>(Position:vector3):navigation_target
    # AI::MakeNavigationTarget<constructor><native><public>(Target:agent):navigation_target
    # AI::(InCharacter:fort_character).GetNavigatable<native><public>()<transacts><decides>:navigatable
    PlayerAgentAI():void=
        Print("Player / Agent / AI")

        MakeNavigationTarget(SomeVec3)

        if (LocalPlayer := MyPlayer?):
            if (LocalPlayer.IsSpectator[]):

            MakeNavigationTarget(LocalPlayer)
            LocalPlayer.GetPlayersSpectating()

            MyRotation := MakeRotation(SomeVec3, 1.0)
            LocalPlayer.Respawn(SomeVec3, MyRotation)

            InstigatorAction := LocalPlayer.GetInstigator()
            if (InstigatorAction.GetInstigatorAgent[]):

            if (FortChar := LocalPlayer.GetFortCharacter[]):
                if (FortChar.GetVehicle[]):
                if (FortChar.GetFocusInterface[]):
                if (FortChar.GetFortLeashable[]):
                if (FortChar.GetNavigatable[]):
                if (FortChar.TeleportTo[FortChar.GetViewLocation() + SomeVec3, FortChar.GetViewRotation()]):
                if (FortChar.TeleportTo[SomeVec3, MyRotation]):

            # LocalPlayer.SendToLobby() # ends session needs manual testing

    # Temporary::UI::texture_block<native><public> := class(widget):
    # Temporary::UI::texture_block::SetImage<native><public>(InImage:texture):void
    # Temporary::UI::texture_block::GetImage<native><public>():texture
    # Temporary::UI::texture_block::SetTint<native><public>(InColor:color):void
    # Temporary::UI::texture_block::GetTint<native><public>():color
    # Temporary::UI::texture_block::SetDesiredSize<native><public>(InDesiredSize:vector2):void
    # Temporary::UI::texture_block::GetDesiredSize<native><public>():vector2
    # Temporary::UI::texture_block::SetTiling<native><public>(InHorizontalTiling:image_tiling, InVerticalTiling:image_tiling):void
    # Temporary::UI::texture_block::GetTiling<native><public>():tuple(image_tiling, image_tiling)
    TextureBlock():void=
        Print("TextureBlock")

        # these textures come from Assets.digest.verse
        MyTextureBlock := texture_block{DefaultImage:=Textures.VerseDeviceStarter_GlassDoor_N}
        MyTextureBlock.SetImage(Textures.Scifi_Building_Gradient)
        MyTextureBlock.GetImage()
        MyTextureBlock.SetTint(color{R := 1.0, G := 0.0, B:= 0.0})
        MyTextureBlock.GetTint()
        MyTextureBlock.SetDesiredSize(SomeVec2)
        MyTextureBlock.GetDesiredSize()
        MyTextureBlock.SetTiling(image_tiling.Stretch, image_tiling.Repeat)
        MyTextureBlock.GetTiling()

    # Temporary::Diagnostics::log::(log:)Print<native><public>(Message:[]char, ?Level:log_level = external {})<computes>:void
    # Temporary::Diagnostics::log::PrintCallStack<native><public>(?Level:log_level = external {})<computes>:void
    LogTest():void=
        # HitSet crash SOL-5235
        Print("Log")

        MyLog := log{Channel := MyLogChannel}
        MyLog.Print("Testing")
        MyLog.PrintCallStack()

    # Temporary::Curves::editable_curve::Evaluate<native><public>(Time:float):float
    Curves():void=
        Print("Curves")

        MyCurve := editable_curve{}
        MyCurve.Evaluate(1.0)

    # Temporary::Diagnostics::debug_draw::ShowChannel<native><public>()<transacts>:void
    # Temporary::Diagnostics::debug_draw::HideChannel<native><public>()<transacts>:void
    # Temporary::Diagnostics::debug_draw::ClearChannel<native><public>()<transacts>:void
    # Temporary::Diagnostics::debug_draw::Clear<native><public>()<transacts>:void
    # Temporary::Diagnostics::debug_draw::DrawSphere<native><public>(Center:vector3, ?Radius:float = external {}, ?Color:color = external {}, ?NumSegments:int = external {}, ?Thickness:float = external {}, ?DrawDurationPolicy:debug_draw_duration_policy = external {}, ?Duration:float = external {})<transacts>:void
    # Temporary::Diagnostics::debug_draw::DrawBox<native><public>(Center:vector3, Rotation:rotation, ?Extent:vector3 = external {}, ?Color:color = external {}, ?Thickness:float = external {}, ?DrawDurationPolicy:debug_draw_duration_policy = external {}, ?Duration:float = external {})<transacts>:void
    # Temporary::Diagnostics::debug_draw::DrawCapsule<native><public>(Center:vector3, Rotation:rotation, ?Height:float = external {}, ?Radius:float = external {}, ?Color:color = external {}, ?Thickness:float = external {}, ?DrawDurationPolicy:debug_draw_duration_policy = external {}, ?Duration:float = external {})<transacts>:void
    # Temporary::Diagnostics::debug_draw::DrawCone<native><public>(Origin:vector3, Direction:vector3, ?Height:float = external {}, ?NumSides:int = external {}, ?AngleWidthRadians:float = external {}, ?AngleHeightRadians:float = external {}, ?Color:color = external {}, ?Thickness:float = external {}, ?DrawDurationPolicy:debug_draw_duration_policy = external {}, ?Duration:float = external {})<transacts>:void
    # Temporary::Diagnostics::debug_draw::DrawCylinder<native><public>(Start:vector3, End:vector3, ?NumSegments:int = external {}, ?Radius:float = external {}, ?Color:color = external {}, ?Thickness:float = external {}, ?DrawDurationPolicy:debug_draw_duration_policy = external {}, ?Duration:float = external {})<transacts>:void
    # Temporary::Diagnostics::debug_draw::DrawLine<native><public>(Start:vector3, End:vector3, ?Color:color = external {}, ?Thickness:float = external {}, ?DrawDurationPolicy:debug_draw_duration_policy = external {}, ?Duration:float = external {})<transacts>:void
    # Temporary::Diagnostics::debug_draw::DrawPoint<native><public>(Position:vector3, ?Color:color = external {}, ?Thickness:float = external {}, ?DrawDurationPolicy:debug_draw_duration_policy = external {}, ?Duration:float = external {})<transacts>:void
    # Temporary::Diagnostics::debug_draw::DrawArrow<native><public>(Start:vector3, End:vector3, ?ArrowSize:float = external {}, ?Color:color = external {}, ?Thickness:float = external {}, ?DrawDurationPolicy:debug_draw_duration_policy = external {}, ?Duration:float = external {})<transacts>:void
    DebugDraw():void=
        Print("DebugDraw")

        MyRotation := MakeRotation(SomeVec3, 1.0)

        MyDebugDraw := debug_draw{}
        MyDebugDraw.ShowChannel()
        MyDebugDraw.HideChannel()
        MyDebugDraw.ClearChannel()
        MyDebugDraw.Clear()
        MyDebugDraw.DrawSphere(SomeVec3)
        MyDebugDraw.DrawBox(SomeVec3, MyRotation)
        MyDebugDraw.DrawCapsule(SomeVec3, MyRotation)
        MyDebugDraw.DrawCone(SomeVec3, SomeVec3)
        MyDebugDraw.DrawCylinder(SomeVec3, SomeVec3)
        MyDebugDraw.DrawLine(SomeVec3, SomeVec3)
        MyDebugDraw.DrawPoint(SomeVec3)
        MyDebugDraw.DrawArrow(SomeVec3, SomeVec3)

    # Concurrency::task<native><public>(t:type)<computes> := class<abstract><final>(awaitable(t)):
    # Concurrency::task::Await<native><override>()<suspends>:t
    Concurrency()<suspends>:void=
        Print("Concurrency")

        MyTask : task(void) = spawn{SuspendsFunction()}
        MyTask.Await()

    # Verse::event<native><public>(t:type)<computes> := class(signalable(t), awaitable(t)):
    # Verse::event::Await<native><override>()<suspends>:t
    # Verse::event::Signal<native><override>(Val:t):void
    Event():void=
        Print("Event")

        spawn{ EventRun() }

    # Verse::Err<native><public>(Message:[]char)<computes>:false
    ErrorTest():void=
        Print("Error")

        # halts the testing, how should we test this? Manually make sure it asserts correctly?
        Err("Error Testing")

    # Devices::CreativeAnimation::animation_controller::AwaitNextKeyframe<native><public>()<suspends>:await_next_keyframe_result
    # Devices::CreativeAnimation::animation_controller::GetState<native><public>()<transacts>:animation_controller_state
    # Devices::CreativeAnimation::animation_controller::IsValid<native><public>()<transacts><decides>:void
    AnimationTest()<suspends>:void=
        Print("Animation")

        MyRotation := MakeRotation(SomeVec3, 1.0)
        MySpawnedProp := SpawnProp(DefaultCreativePropAsset, SomeVec3, MyRotation)

        if (MyOtherProp := MySpawnedProp(0)?):
            Print(" Spawend Prop ")
            if (MyOtherProp.IsValid[]):
                Print(" Spawend Prop Valid")

                if (MyAnimationController := MyOtherProp.GetAnimationController[]):
                    Print("  AnimationController")

                    if (MyAnimationController.IsValid[]):
                        Print("    AnimationController IsValid")

                        MyAnimationController.GetState()
                        MyAnimationController.AwaitNextKeyframe()

    # Devices::SpawnProp<native><public>(Asset:creative_prop_asset, Position:vector3, Rotation:rotation)<transacts>:tuple(?creative_prop, spawn_prop_result)
    # Devices::SpawnProp<native><public>(Asset:creative_prop_asset, Transform:transform)<transacts>:tuple(?creative_prop, spawn_prop_result)
    SpawnPropTest():void=
        Print("SpawnProp")

        MyRotation := MakeRotation(SomeVec3, 1.0)
        SomeTransform := transform{Scale:=SomeVec3, Rotation:=MyRotation, Translation:=SomeVec3}

        # a ton of transaction failures
        SpawnProp(DefaultCreativePropAsset, SomeVec3, MyRotation)
        SpawnProp(DefaultCreativePropAsset, SomeTransform)

    # Simulation::GetSession<native><public>()<varies>:session
    # Simulation::Sleep<native><public>(Seconds:float)<suspends>:void
    # Simulation::GetSimulationElapsedTime<native><public>()<transacts>:float
    SimulationTest()<suspends>:void=
        Print("Simulation")

        # SOL-5347
        GetSession()
        Sleep(1.0)
        GetSimulationElapsedTime()

    # UI::(Playspace:fort_playspace).GetHUDController<native><public>():fort_hud_controller
    HudController():void=
        Print("HudController")
 
        # HitSet crash SOL-5235
        FortHud := Self.GetPlayspace().GetHUDController()

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=

        # Verse::Print<native><public>(Message:[]char, ?Duration:float = external {}, ?Color:color = external {}):void
        Print("------- Start of Tests -------")
 
        set MyPlayer = option:
            Players := Self.GetPlayspace().GetPlayers()
            Players[Players.Length - 1]

        Silder()
        TextBlock()
        Button()
        Overlay()
        StackBox()
        PlayerUI()
        Widget()
        TextBase()
        Colors()
        ColorBlock()
        Canvas()
        SpawnPropTest()
        CreativeDevice()
        CreativeProp()
        CreativeObjectAndTags()
        Math()
        Random()
        ToStringTest()
        SpatialMath()
        LocalizeTest()
        InvalidatableTest()
        StringTest()
        PlayerAgentAI()
        TextureBlock()
        Curves()
        LogTest()
        DebugDraw()
        AnimationTest()
        SimulationTest()
        HudController()

        # ErrorTest() # Stops this program, so should be manually tested

        # Run these tests last, transaction failures didnt seem to hit again after these ran? TODO figure out why
        Concurrency()
        Event()

        Print("------- End of Tests -------")