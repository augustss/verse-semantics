using { /Fortnite.com/Devices }
using { /Fortnite.com/UI }
using { /Verse.org/Simulation }
using { /Verse.org/Colors }
using { /Verse.org/Random }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/UI }
using { /UnrealEngine.com/Temporary/SpatialMath}

first_bonus_card<public> := class():
    BonusText<public>:text_block := text_block {
        DefaultTextColor := NamedColors.White
        DefaultTextOpacity := 1.0
        DefaultJustification := text_justification.Center
    }

    Button<public>:button_regular := button_regular{}

    ColorBlock<public>:color_block := color_block{
        DefaultColor := NamedColors.Black
        DefaultOpacity := 1.0
        DefaultDesiredSize := vector2{X:= 300.0, Y:= 200.0}
    }

second_bonus_card<public> := class():
    BonusText<public>:text_block := text_block {
        DefaultTextColor := NamedColors.White
        DefaultTextOpacity := 1.0
        DefaultJustification := text_justification.Center
    }

    Button<public>:button_regular := button_regular{}

    ColorBlock<public>:color_block := color_block{
        DefaultColor := NamedColors.Black
        DefaultOpacity := 1.0
        DefaultDesiredSize := vector2{X:= 300.0, Y:= 200.0}
    }

third_bonus_card<public> := class():
    BonusText<public>:text_block := text_block {
        DefaultTextColor := NamedColors.White
        DefaultTextOpacity := 1.0
        DefaultJustification := text_justification.Center
    }

    Button<public>:button_regular := button_regular{}

    ColorBlock<public>:color_block := color_block{
        DefaultColor := NamedColors.Black
        DefaultOpacity := 1.0
        DefaultDesiredSize := vector2{X:= 300.0, Y:= 200.0}
    }

hud_device := class(creative_device):

    @editable
    TimeStopper:cinematic_sequence_device := cinematic_sequence_device{}
    @editable
    BonusManager:bonus_manager := bonus_manager{}
    @editable
    PlayerManager:player_manager := player_manager{}
    @editable
    Utils:utils := utils{}
    @editable
    Bus:vehicle_spawner_armored_battle_bus_device := vehicle_spawner_armored_battle_bus_device{}

    var PlayerUI<internal>:?player_ui = false


    FirstCard : first_bonus_card := first_bonus_card{}
    SecondCard : second_bonus_card := second_bonus_card{}
    ThirdCard : third_bonus_card := third_bonus_card{}
    
    var maybe_first_card_bonus:?bonus := false
    var maybe_second_card_bonus:?bonus := false
    var maybe_third_card_bonus:?bonus := false

    Slider<private>:slider_regular := slider_regular{
        DefaultValue := 0.0
        DefaultMinValue := 0.0
        DefaultMaxValue := 10.0
        DefaultStepSize := 1.0
    }

    LevelText<private>:text_block := text_block {
        DefaultTextColor := NamedColors.White
        DefaultTextOpacity := 0.9
        DefaultJustification := text_justification.Left
        DefaultOverflowPolicy := text_overflow_policy.Clip
    }
    
    HideHUDElementsArray<private>:[]hud_element_identifier = array{hud_identifier_world_resource_wood{}, hud_identifier_world_resource_stone{}, hud_identifier_world_resource_metal{},
    hud_identifier_world_resource_permanite{}, hud_identifier_world_resource_gold_currency{}, hud_identifier_world_resource_ingredient{}, creative_hud_identifier_build_menu{},
    creative_hud_identifier_player_inventory{}}
    
    HideAdditionalHUDElementsArray<private>:[]hud_element_identifier = array{creative_hud_identifier_mimimap{}, creative_hud_identifier_round_info{}}
    
    HUDOnStartSetting<public>()<suspends>:void=
        Utils.PlayerSpawnedEvent.Await()
        if(C:= PlayerManager.Character?):
            set PlayerUI = option{GetPlayerUI[player[C.GetAgent[]]]}
        LevelVL := LevelVLInitilize()
        UISlot:player_ui_slot := player_ui_slot { InputMode := ui_input_mode.None }
        if (TmpPlayerUI := PlayerUI?):
            TmpPlayerUI.HideHUDElements(HideHUDElementsArray)
            TmpPlayerUI.HideHUDElements(HideAdditionalHUDElementsArray)
            TmpPlayerUI.AddWidget(LevelVL, UISlot)

    LevelVLInitilize<private>():canvas=
        LevelText.SetText(Utils.MyMessageWithArgs("Level {PlayerManager.PlayerLevel}"))

        CanvasSlot1 :=  MakeCanvasSlot(Widget:= Slider, Position:= vector2{X := 700.0, Y := 80.0} , ?Size:= vector2{X := 550.0, Y := 200.0})
        CanvasSlot2 :=  MakeCanvasSlot(Widget1 := LevelText, Position1:= vector2{X := 700.0, Y := 30.0} , ?Size:= vector2{X := 500.0, Y := 200.0})

        LevelVL := canvas{}
        LevelVL.AddWidget(CanvasSlot1)
        LevelVL.AddWidget(CanvasSlot2)

        return LevelVL

    BonusVLInitilize<private>():canvas=
        BonusToCardRandomisation()
        
        CanvasSlot1 :=  MakeCanvasSlot(Widget:= FirstCard.ColorBlock, Position:= vector2{X := 400.0, Y := 80.0} , ?Size:= vector2{X := 300.0, Y := 300.0})
        CanvasSlot2 :=  MakeCanvasSlot(Widget1 := FirstCard.BonusText, Position1:= vector2{X := 500.0, Y := 80.0} , ?Size:= vector2{X := 100.0, Y := 300.0})
        CanvasSlot3 :=  MakeCanvasSlot(Widget2:= FirstCard.Button, Position2:= vector2{X := 425.0, Y := 300.0} , ?Size:= vector2{X := 250.0, Y := 50.0})
        
        CanvasSlot4 :=  MakeCanvasSlot(Widget3:= SecondCard.ColorBlock, Position3:= vector2{X := 800.0, Y := 80.0} , ?Size:= vector2{X := 300.0, Y := 300.0})
        CanvasSlot5 :=  MakeCanvasSlot(Widget4:= SecondCard.BonusText, Position4:= vector2{X := 900.0, Y := 80.0} , ?Size:= vector2{X := 100.0, Y := 300.0})
        CanvasSlot6 :=  MakeCanvasSlot(Widget5 := SecondCard.Button, Position5:= vector2{X := 825.0, Y := 300.0} , ?Size:= vector2{X := 250.0, Y := 50.0})
       
        
        CanvasSlot7 :=  MakeCanvasSlot(Widget6:= ThirdCard.ColorBlock, Position6:= vector2{X := 1200.0, Y := 80.0} , ?Size:= vector2{X := 300.0, Y := 300.0})
        CanvasSlot8 :=  MakeCanvasSlot(Widget7 := ThirdCard.BonusText, Position7:= vector2{X := 1300.0, Y := 80.0} , ?Size:= vector2{X := 100.0, Y := 300.0})
        CanvasSlot9 :=  MakeCanvasSlot(Widget8:= ThirdCard.Button, Position8:= vector2{X := 1225.0, Y := 300.0} , ?Size:= vector2{X := 250.0, Y := 50.0})
        

        BonusVL := canvas{}
        BonusVL.AddWidget(CanvasSlot1)
        BonusVL.AddWidget(CanvasSlot2)
        BonusVL.AddWidget(CanvasSlot3)
       
        BonusVL.AddWidget(CanvasSlot4)
        BonusVL.AddWidget(CanvasSlot5)
        BonusVL.AddWidget(CanvasSlot6)
        
        BonusVL.AddWidget(CanvasSlot7)
        BonusVL.AddWidget(CanvasSlot8)
        BonusVL.AddWidget(CanvasSlot9)

        FirstCard.Button.OnClick().Subscribe(OnFirstUIButtonClicked)
        SecondCard.Button.OnClick().Subscribe(OnSecondUIButtonClicked)
        ThirdCard.Button.OnClick().Subscribe(OnThirdUIButtonClicked)

        return BonusVL

    SliderAdding<public>():void=
        CurrentValue := Slider.GetValue()
        MaxValue := Slider.GetMaxValue()

        if(CurrentValue = MaxValue - 1.0):
            Slider.SetValue(Slider.GetMinValue())
            set PlayerManager.PlayerLevel = PlayerManager.PlayerLevel + 1
            LevelText.SetText(Utils.MyMessageWithArgs("Level {PlayerManager.PlayerLevel}"))
            BonusCanvasLogic()
        else:
            Slider.SetValue(CurrentValue + 1.0)

    BonusCanvasLogic<public>():void=
        TimeStopper.Play()
        BonusVL := BonusVLInitilize()
        UISlot:player_ui_slot := player_ui_slot { InputMode := ui_input_mode.All }
        if (TmpPlayerUI := PlayerUI?):
            TmpPlayerUI.AddWidget(BonusVL, UISlot)

    OnFirstUIButtonClicked(WidgetOutput:widget_message):void=
        Player := PlayerManager.Character
        ItemGranter := PlayerManager.ItemGranter
        if(Bonus := maybe_first_card_bonus?):
            if(P := Player?):
                Bonus.GiveBonus(P, ItemGranter, Bus)
            else:
        TimeStopper.Stop()
        if (Parent := WidgetOutput.Source.GetParentWidget[]):
            if (TmpPlayerUI := GetPlayerUI[WidgetOutput.Player]):
                TmpPlayerUI.RemoveWidget(Parent) 
    
    OnSecondUIButtonClicked(WidgetOutput:widget_message):void=
        Player := PlayerManager.Character
        ItemGranter := PlayerManager.ItemGranter
        if(Bonus := maybe_second_card_bonus?):
            if(P := Player?):
                    Bonus.GiveBonus(P, ItemGranter,Bus)
        TimeStopper.Stop()
        if (Parent := WidgetOutput.Source.GetParentWidget[]):
            if (TmpPlayerUI := GetPlayerUI[WidgetOutput.Player]):
                TmpPlayerUI.RemoveWidget(Parent) 

    OnThirdUIButtonClicked(WidgetOutput:widget_message):void=
        Player := PlayerManager.Character
        ItemGranter := PlayerManager.ItemGranter
        if(Bonus := maybe_third_card_bonus?):
            if(P := Player?):
                Bonus.GiveBonus(P, ItemGranter, Bus)
        TimeStopper.Stop()
        if (Parent := WidgetOutput.Source.GetParentWidget[]):
            if (TmpPlayerUI := GetPlayerUI[WidgetOutput.Player]):
                TmpPlayerUI.RemoveWidget(Parent) 
                
   
    BonusToCardRandomisation():void=
        MessWrap := Utils.MyMessageWithArgs
        FirstCardText := FirstCard.BonusText
        FirstCardColor := FirstCard.ColorBlock
       
        SecondCardText := SecondCard.BonusText
        SecondCardColor := SecondCard.ColorBlock

        ThirdCardText := ThirdCard.BonusText
        ThirdCardColor := ThirdCard.ColorBlock

        Print("Bonus to card randomizing started")

        if:
            first_card_bonus := BonusManager.Bonuses[GetRandomInt(0, BonusManager.Bonuses.Length - 1)]
            set maybe_first_card_bonus = option{first_card_bonus} 
            FirstBonusText := first_card_bonus.bonus_string    
            FirstBonusColor := first_card_bonus.bonus_color
            
            var SecondBonusText:string := ""
            var SecondBonusColor:color := NamedColors.White
            var SecondBonusBool:logic := false
           
            loop:
                if:
                    C := BonusManager.Bonuses[GetRandomInt(0, BonusManager.Bonuses.Length - 1)]
                then:
                    if(first_card_bonus <> C):
                        set maybe_second_card_bonus = option{C}
                        break

            if(second_bonus := maybe_second_card_bonus?):
                set SecondBonusText = second_bonus.bonus_string
                set SecondBonusColor = second_bonus.bonus_color
                set SecondBonusBool = second_bonus.is_repeated

            var ThirdBonusText:string := ""
            var ThirdBonusColor:color := NamedColors.White
            var ThirddBonusBool:logic := false

            loop:
                if:
                    C := BonusManager.Bonuses[GetRandomInt(0, BonusManager.Bonuses.Length - 1)]
                then:
                    if(second_card_bonus := maybe_second_card_bonus?):
                        if(first_card_bonus <> C and second_card_bonus <> C):
                            set maybe_third_card_bonus = option{C}
                            break
                            
            if(third_card_bonus := maybe_third_card_bonus?):
                set ThirdBonusText = third_card_bonus.bonus_string
                set ThirdBonusColor = third_card_bonus.bonus_color
                set ThirddBonusBool = third_card_bonus.is_repeated

        then:
            FirstCardText.SetText(MessWrap(FirstBonusText))
            FirstCardColor.SetColor(FirstBonusColor)
            Print("First card ready")
            if(first_card_bonus.is_repeated = false):
                if:
                    BonusManager.Bonuses.RemoveElement[first_card_bonus.id]
            
            SecondCardText.SetText(MessWrap(SecondBonusText))
            SecondCardColor.SetColor(SecondBonusColor)
            Print("Second card ready")
            if(SecondBonusBool = false):
                if(C := maybe_second_card_bonus?):
                    if:
                        BonusManager.Bonuses.RemoveElement[C.id]

            ThirdCardText.SetText(MessWrap(ThirdBonusText))
            ThirdCardColor.SetColor(ThirdBonusColor)
            Print("Third card ready")
            if(ThirddBonusBool = false):
                if(C := maybe_third_card_bonus?):
                    if:
                        BonusManager.Bonuses.RemoveElement[C.id]

