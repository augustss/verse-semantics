using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/UI }
using { /Fortnite.com/UI }
using { /UnrealEngine.com/Temporary/SpatialMath }

interfaceble := interface(): #Sorry for the bad name
    CreateUI(Text : []char) : canvas
    AddWidget(Agent : agent, Text : []char):void
    RemoveWidget<public>(Agent : agent):void
   
widget_device<internal> := class(creative_device, interfaceble):
    
    var MaybeMyUIPerPlayer<protected> : [player]?canvas = map{}
    TextForUI<localizes><protected>(InText : string) : message = "{InText}"
    
    CreateUI<override>(Text : []char) : canvas =
        MyCanvas : canvas = canvas:
            Slots := array:
                canvas_slot:
                    Anchors := anchors{Minimum := vector2{X := 0.0, Y := 1.0}, Maximum := vector2{X := 0.0, Y := 1.0}}
                    Offsets := margin{Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                    Alignment := vector2{X := 0.0, Y := 1.0}
                    SizeToContent := true
                    Widget := button_loud{DefaultText := TextForUI(Text)}
    
        return MyCanvas
    
    AddWidget<override>(Agent : agent, Text : []char):void=
        if (InPlayer := player[Agent], PlayerUI := GetPlayerUI[InPlayer]):
            if (MyUI := MaybeMyUIPerPlayer[InPlayer]?):
                PlayerUI.RemoveWidget(MyUI)
                if (set MaybeMyUIPerPlayer[InPlayer] = false) {}
            NewUI := CreateUI(Text)
            PlayerUI.AddWidget(NewUI)
            if (set MaybeMyUIPerPlayer[InPlayer] = option{NewUI}) {}
    
    
    RemoveWidget<override>(Agent : agent):void=
        if (InPlayer := player[Agent], PlayerUI := GetPlayerUI[InPlayer]):
            if (MyUI := MaybeMyUIPerPlayer[InPlayer]?):
                PlayerUI.RemoveWidget(MyUI)
                if (set MaybeMyUIPerPlayer[InPlayer] = false) {}    
      
speedometer_device := class(widget_device):
    
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - speedometer_device")

    OnEnd<override>():void=
        Print("Nexus OnEnd - speedometer_device")

collectible_widget := class(widget_device):

    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - collectible_widget")

    OnEnd<override>():void=
        Print("Nexus OnEnd - collectible_widget")

    CreateUI<override>(Text : []char) : canvas =
        CollectiblesCounterWidget : button_regular = button_regular{DefaultText := TextForUI(Text)}

        MyCanvas : canvas = canvas:
            Slots := array:
                canvas_slot:
                    Anchors := anchors{Minimum := vector2{X := 1.0, Y := 1.0}, Maximum := vector2{X := 1.0, Y := 1.0}}
                    Offsets := margin{Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                    Alignment := vector2{X := 1.0, Y := 1.0}
                    SizeToContent := true
                    Widget := CollectiblesCounterWidget

        return MyCanvas

CreateCollectibleWidget<constructor>() := collectible_widget: