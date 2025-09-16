
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/UI }
using { /Verse.org/Colors }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Fortnite.com/UI }
# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

# A Verse-authored creative device that can be placed in a level
hud_device := class(creative_device):

    var MaybeMyUIPerPlayer:[player]?canvas = map{}
    var TimerSliders:[]slider_regular=array{}
    TimerSliderMessage<localizes>():message = "Time Left: "
    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        # TODO: Replace this with your code
        Print("Hello, world!")
        Print("2 + 2 = {2 + 2}")

    
    SurvivalUI():canvas=


        var NewXPBarSlider : slider_regular= slider_regular{}

        NewCanvas : canvas = canvas:
            Slots := array:
                canvas_slot: 
                    Widget := overlay:
                        Slots := array:
                            overlay_slot:
                                Widget := color_block:
                                    DefaultColor := NamedColors.Crimson
                                    DefaultOpacity := 0.75
                                    DefaultDesiredSize := vector2{X := 606.6, Y := 128.0}                                
                            overlay_slot:
                                        Widget := stack_box:
                                            Orientation := orientation.Horizontal
                                            Slots := array:
                                                stack_box_slot:
                                                    Widget := text_block{
                                                        DefaultText := TimerSliderMessage(),
                                                        DefaultTextColor := MakeColorFromSRGB(
                                                            Red:float:=0.25, 
                                                            Green:float:=1.0, 
                                                            Blue:float:=1.0)
                                                            }
                                                stack_box_slot:
                                                    Widget := set NewXPBarSlider = slider_regular:
                                                        DefaultValue := 0.0
                                                        DefaultMinValue := 0.0
                                                        DefaultMaxValue := 5.0 
                                                        DefaultStepSize := 0.5
                    # Overlay position
                    Anchors := anchors{Minimum := vector2{X := 1.0, Y := 0.5}, Maximum := vector2{X := 1.0, Y := 0.5}}
                    Offsets := margin{Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                    Alignment := vector2{X := 1.0, Y := 0.5}
                    SizeToContent := true     
        # Canvas position
        Anchors := anchors{Minimum := vector2{X := 0.5, Y := 0.5}, Maximum := vector2{X := 0.5, Y := 0.5}}
        Offsets := margin{Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
        Alignment := vector2{X := 0.5, Y := 0.5}
        SizeToContent := true
        set TimerSliders += array{NewXPBarSlider}
        return NewCanvas


    AddPlayerUI<public>(Agent : agent) : void =

        if (InPlayer := player[Agent], PlayerUI := GetPlayerUI[InPlayer]):
            if (MyUI := MaybeMyUIPerPlayer[InPlayer]?):
                PlayerUI.RemoveWidget(MyUI)
                if (set MaybeMyUIPerPlayer[InPlayer] = false) {}
            else:
                NewUI := SurvivalUI()
                PlayerUI.AddWidget(NewUI)
                if (set MaybeMyUIPerPlayer[InPlayer] = option{NewUI}) {}
                
   
                NewQuestUI := SurvivalUI()
                PlayerUI.AddWidget(SurvivalUI())

    RemovePlayerUI<public>(Agent : agent) : void =

        if (InPlayer := player[Agent], PlayerUI := GetPlayerUI[InPlayer]):
            if (MyUI := MaybeMyUIPerPlayer[InPlayer]?):
                PlayerUI.RemoveWidget(MyUI)
                if (set MaybeMyUIPerPlayer[InPlayer] = false) {}
           

    UpdateTimerSlider(TimeFloat:float):void=

        for(Slider:TimerSliders):
            Slider.SetValue(TimeFloat)