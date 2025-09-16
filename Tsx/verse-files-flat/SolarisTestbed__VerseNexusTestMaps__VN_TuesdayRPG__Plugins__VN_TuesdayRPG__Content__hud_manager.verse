
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /Fortnite.com/UI }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/UI }
using { /Verse.org/Assets }
using { /Fortnite.com/Characters }
using { /Verse.org/Colors }


hud_manager := class(creative_device):

    ShadowOffset : ?vector2 = option{vector2{X:=2.0,Y:=-2.0}}


    LevelUpTexture: texture = Textures.LevelUpTexture

    @editable
    MyButton : button_device = button_device{}

    TextSpacing<localizes> : message = "      "

    # Map storage for player UI
    var MaybeMyUIPerPlayer : [player]?canvas = map{}
    var MaybeMyLevelUpUIPerPlayer : [player]?canvas = map{}

    LevelUpMessage<localizes> : message = "Level Up!"
    AddHealthMessage<localizes> : message = "+25 Health"
    AddShieldMessage<localizes> : message = "+15 Shield"

    
    XPMessage<localizes>(XPLeft:int) : message = "XP Left: {XPLeft}"
    LevelMessage<localizes>(Level:int) : message = "Lvl: {Level}"
    GoldMessage<localizes>(Gold:int) : message = "Gold: {Gold}"

    var XPTextBlock:text_block=text_block{}
    var LevelTextBlock:text_block=text_block{}
    var GoldTextBlock:text_block=text_block{}

    # Parallel Array Storage of player specific UI
    var XPTextBlocks:[]text_block=array{}
    var LevelTextBlocks:[]text_block=array{}
    var GoldTextBlocks:[]text_block=array{}
    var XPBarSliders:[]slider_regular=array{}

    # Quest array that is updated simultaneously through iteration
    var QuestTextBlocks:[]text_block = array{}

    # Quest stuff
    QuestBoxTitle<localizes> : message = "Quest Info:"
    QuestMessage<localizes>(EnemyString:string,QuestEnemiesEliminated:int, TotalQuestEnemiesToEliminate:int) : message = "Elimininate {EnemyString}s: {QuestEnemiesEliminated}/{TotalQuestEnemiesToEliminate}"
    QuestsCompletedMessage<localizes> : message = "Congrats! All quests complete!"

    # var QuestEnemiesTextBlock:text_block=text_block{}
    var PlayerQuestUI:canvas = canvas{}

    CreateQuestUI(): tuple(canvas, text_block) =

        var NewQuestTextBlock: text_block = text_block{}

        QuestCanvas : canvas = canvas:
            Slots := array:
                canvas_slot:

                    Widget := overlay:
                        Slots := array:
                            overlay_slot:
                                Widget := color_block:
                                    DefaultColor := NamedColors.DarkSlateGray
                                    DefaultOpacity := 0.75
                                    DefaultDesiredSize := vector2{X := 520.0, Y := 76.6}                                
                            overlay_slot:
                                Widget := stack_box:
                                    Orientation := orientation.Vertical
                                    Slots := array:
                                        stack_box_slot:
                                            Widget := TextBlock := text_block{
                                                DefaultText :=  QuestBoxTitle,
                                                DefaultTextColor := MakeColorFromSRGB(
                                                    Red:float:=1.0, 
                                                    Green:float:=1.0, 
                                                    Blue:float:=1.0),
                                                DefaultShadowOffset := ShadowOffset,
                                                DefaultShadowColor := NamedColors.Black
                                                # DefaultJustification := text_justification.Center
                                                    }  
                                        stack_box_slot:
                                            Widget :=  set NewQuestTextBlock = text_block{
                                                DefaultText :=  QuestMessage("thingy mobobbers",0,0),
                                                DefaultTextColor := MakeColorFromSRGB(
                                                    Red:float:=1.0, 
                                                    Green:float:=1.0, 
                                                    Blue:float:=1.0)
                                                    # DefaultJustification := text_justification.Center
                                                    # DefaultShadowOffset := ShadowOffset
                                                    }       
                    Anchors := anchors{Minimum := vector2{X := 1.0, Y := 0.5}, Maximum := vector2{X := 1.0, Y := 0.5}}
                    Offsets := margin{Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                    Alignment := vector2{X := 1.0, Y := 0.5}
                    SizeToContent := true
        Anchors := anchors{Minimum := vector2{X := 0.5, Y := 0.0}, Maximum := vector2{X := 0.5, Y := 0.0} }
        Offsets := margin{Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
        Alignment := vector2{X := 0.5, Y := 0.0}
        SizeToContent := true

        set PlayerQuestUI = QuestCanvas
        QuestCanvas, NewQuestTextBlock

    CreateMyLevelUpUI(PlayerID:int): canvas =

        AddShieldButton : button_regular = button_regular{DefaultText := AddShieldMessage}
        AddHealthButton : button_regular = button_regular{DefaultText := AddHealthMessage}

        AddHealthButton.OnClick().Subscribe(HandleAddHealthButtonPress)
        AddShieldButton.OnClick().Subscribe(HandleAddShieldButtonPress)

        LevelUpCanvas : canvas = canvas:
            Slots := array:
                canvas_slot: 
                    Widget := overlay:
                        Slots := array:
                            overlay_slot:
                                Widget := color_block:
                                    DefaultColor := NamedColors.DarkSlateGray
                                    DefaultOpacity := 0.75
                                    DefaultDesiredSize := vector2{X := 520.0, Y := 106.6}                                
                            overlay_slot:
                                Widget := stack_box:
                                    Orientation := orientation.Vertical
                                    Slots := array:
                                        stack_box_slot: # Top
                                            Widget := stack_box:
                                                Orientation := orientation.Horizontal
                                                Slots := array:
                                                    stack_box_slot:
                                                        # Widget :=  color_block{DefaultColor:=MakeColorFromSRGB(
                                                        #     Red:float:=1.0, 
                                                        #     Green:float:=1.0, 
                                                        #     Blue:float:=1.0)}
                                                        Widget := texture_block{DefaultImage := LevelUpTexture}
                                                    stack_box_slot:
                                                        Widget :=  text_block{
                                                            DefaultText := LevelUpMessage,
                                                            DefaultTextColor := MakeColorFromSRGB(
                                                                Red:float:=1.0, 
                                                                Green:float:=1.0, 
                                                                Blue:float:=1.0)
                                                                # DefaultShadowOffset := ShadowOffset
                                                                }
                                                            # SetShadowOffset(vector2{x:=1, y:=1})
                                        stack_box_slot: # Bottom
                                            Widget := stack_box:
                                                Orientation := orientation.Horizontal
                                                Slots := array:
                                                    stack_box_slot:
                                                        Widget := AddHealthButton
                                                    stack_box_slot:
                                                        Widget := AddShieldButton
                    Anchors := anchors{Minimum := vector2{X := 0.5, Y := 0.5}, Maximum := vector2{X := 0.5, Y := 0.5}}
                    Offsets := margin{Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                    Alignment := vector2{X := 0.5, Y := 0.5}
                    SizeToContent := true

        Anchors := anchors{Minimum := vector2{X := 0.5, Y := 0.5}, Maximum := vector2{X := 0.5, Y := 0.5}}
        Offsets := margin{Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
        Alignment := vector2{X := 0.5, Y := 0.5}
        SizeToContent := true
        LevelUpCanvas                    


    CreateMyUI(XPInt:int,MaxXPInt:int,LevelInt:int,GoldInt:int) : tuple(canvas, text_block, text_block,text_block,slider_regular) =

        var NewLevelTextBlock: text_block = text_block{}
        var NewXPTextBlock: text_block = text_block{}
        var NewGoldTextBlock: text_block = text_block{}
        var NewXPBarSlider:slider_regular=slider_regular{}

        MyCanvas : canvas = canvas:
            Slots := array:
                canvas_slot: 
                    Widget := overlay:
                        Slots := array:
                            overlay_slot:
                                Widget := color_block:
                                    DefaultColor := NamedColors.DarkSlateGray
                                    DefaultOpacity := 0.75
                                    DefaultDesiredSize := vector2{X := 606.6, Y := 128.0}                                
                            overlay_slot:
                                        Widget := stack_box:
                                            Orientation := orientation.Vertical
                                            Slots := array:
                                                stack_box_slot:
                                                    Widget := stack_box:
                                                        Orientation := orientation.Horizontal
                                                        Slots := array:
                                                            stack_box_slot:
                                                                Widget := set NewLevelTextBlock = text_block{
                                                                    DefaultText := LevelMessage(LevelInt),
                                                                    DefaultTextColor := MakeColorFromSRGB(
                                                                        Red:float:=0.25, 
                                                                        Green:float:=1.0, 
                                                                        Blue:float:=1.0)
                                                                        # DefaultShadowOffset := ShadowOffset
                                                                        # DefaultTextOpacity := 1.0
                                                                        }
                                                            stack_box_slot: # SPACING
                                                                Widget := text_block{
                                                                    DefaultText := TextSpacing}
                                                            stack_box_slot:
                                                                Widget := set NewXPTextBlock = text_block{
                                                                    DefaultText := XPMessage(MaxXPInt-XPInt),
                                                                    DefaultTextColor := MakeColorFromSRGB(
                                                                        Red:float:=1.0, 
                                                                        Green:float:=0.25, 
                                                                        Blue:float:=1.0)
                                                                        # DefaultShadowOffset := ShadowOffset
                                                                        # DefaultTextOpacity := 1.0
                                                                        }
                                                            stack_box_slot: # SPACING
                                                                Widget := text_block{DefaultText := TextSpacing}
                                                            stack_box_slot:
                                                                Widget := set NewGoldTextBlock= text_block{
                                                                    DefaultText := GoldMessage(GoldInt),
                                                                    DefaultTextColor := MakeColorFromSRGB(
                                                                        Red:float:=1.0, 
                                                                        Green:float:=1.0, 
                                                                        Blue:float:=0.0)
                                                                    # DefaultShadowOffset := ShadowOffset
                                                                    # DefaultTextOpacity := 1.0
                                                                }            

                                                stack_box_slot:
                                                    Widget := set NewXPBarSlider = slider_regular:
                                                        DefaultValue := 0.0
                                                        DefaultMinValue := 0.0
                                                        DefaultMaxValue := 100.0 
                                                        DefaultStepSize := 0.5
                                                        
                    Anchors := anchors{Minimum := vector2{X := 0.5, Y := 1.0}, Maximum := vector2{X := 0.5, Y := 1.0}}
                    Offsets := margin{Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 250.0}
                    Alignment := vector2{X := 0.5, Y := 1.0}
                    SizeToContent := true

        #return MyCanvas
        (MyCanvas,NewXPTextBlock,NewLevelTextBlock,NewGoldTextBlock, NewXPBarSlider)

    AddPlayerUI<public>(Agent : agent, PlayerData:player_data) : void =

        if (InPlayer := player[Agent], PlayerUI := GetPlayerUI[InPlayer]):
            if (MyUI := MaybeMyUIPerPlayer[InPlayer]?):
                PlayerUI.RemoveWidget(MyUI)
                if (set MaybeMyUIPerPlayer[InPlayer] = false) {}
            else:
                NewUI := CreateMyUI(PlayerData.XP,PlayerData.MaxXP,PlayerData.Level,PlayerData.Gold)
                PlayerUI.AddWidget(NewUI(0))
                if (set MaybeMyUIPerPlayer[InPlayer] = option{NewUI(0)}) {}
                NewXPTextBlockArray:[]text_block = array{NewUI(1)}
                NewLevelTextBlockArray:[]text_block = array{NewUI(2)}
                NewGoldTextBlockArray:[]text_block = array{NewUI(3)}
                NewXPBarSliderArray:[]slider_regular = array{NewUI(4)}

                set XPTextBlocks += NewXPTextBlockArray
                set LevelTextBlocks += NewLevelTextBlockArray
                set GoldTextBlocks += NewGoldTextBlockArray
                set XPBarSliders += NewXPBarSliderArray
                NewQuestUI := CreateQuestUI()
                PlayerUI.AddWidget(NewQuestUI(0))
                set QuestTextBlocks += array{NewQuestUI(1)}

    AddLevelUpUI<public>(Agent:agent) : void = 
        if (InPlayer := player[Agent], PlayerUI := GetPlayerUI[InPlayer]):
            if (MyUI := MaybeMyLevelUpUIPerPlayer[InPlayer]?):
                PlayerUI.RemoveWidget(MyUI)
                if (set MaybeMyLevelUpUIPerPlayer[InPlayer] = false) {}
                AddLevelUpUI(Agent)
            else:
                NewUI := CreateMyLevelUpUI(1)
                PlayerUI.AddWidget(NewUI, player_ui_slot{InputMode := ui_input_mode.All})
                if(set MaybeMyLevelUpUIPerPlayer[InPlayer] = option{NewUI}) {}
                #NewLevelUpUI := 

    UpdateXPText<public>(PlayerID:int,XP:int,MaxXP:int):void=
        # XPTextBlock.SetText(XPMessage(XP,MaxXP)) 
        if(UpdatedXPText:text_block =XPTextBlocks[PlayerID]):
            UpdatedXPText.SetText(XPMessage(MaxXP-XP))
            UpdateXPBar(PlayerID,MaxXP,XP)
        
        #{DefaultText := XPMessage(XP,MaxXP),DefaultTextColor := MakeColorFromSRGB(Red:float:=1.0, Green:float:=0.25, Blue:float:=1.0)}

    UpdateGoldText<public>(PlayerID:int,Gold:int):void=
        # GoldTextBlock.SetText(GoldMessage(Gold)) 
        if(UpdatedGoldText:text_block =GoldTextBlocks[PlayerID]):
            UpdatedGoldText.SetText(GoldMessage(Gold))
        
    UpdateLevelText<public>(PlayerID:int,Level:int):void=
        # LevelTextBlock.SetText(LevelMessage(Level)) 
        if(UpdatedLevelText:text_block =LevelTextBlocks[PlayerID]):
            UpdatedLevelText.SetText(LevelMessage(Level))
    
    UpdateXPBar<public>(PlayerID:int,MaxXP:int,CurrentXP:int):void=

        if(UpdatedXPBar:slider_regular =XPBarSliders[PlayerID]):
            UpdatedXPBar.SetValue(CurrentXP*1.0)
            UpdatedXPBar.SetMaxValue(MaxXP*1.0)

    UpdateQuestInfo<public>(EnemyString:string,QuestEnemiesEliminated:int, TotalQuestEnemiesToEliminate:int):void=
        for(QuestTextBlock : QuestTextBlocks):
            QuestTextBlock.SetText(QuestMessage(EnemyString, QuestEnemiesEliminated, TotalQuestEnemiesToEliminate))

    SetQuestMessagesToCompleted<public>():void=
        for(QuestTextBlock : QuestTextBlocks):
            QuestTextBlock.SetText(QuestsCompletedMessage)

    HandleAddHealthButtonPress(Message : widget_message):void=
        if (PlayerUI := GetPlayerUI[Message.Player], MyUI := MaybeMyLevelUpUIPerPlayer[Message.Player]?, SelectedButton := text_button_base[Message.Source]):
            Print("Player selected {SelectedButton.GetText()}")
            PlayerUI.RemoveWidget(MyUI)
            if (set MaybeMyLevelUpUIPerPlayer[Message.Player] = false) {}
            Player:agent = Message.Player
            if(FortChar:fort_character = Player.GetFortCharacter[]):
                FortChar.SetMaxHealth(FortChar.GetMaxHealth() + 25.0)

    HandleAddShieldButtonPress(Message : widget_message):void=
        if (PlayerUI := GetPlayerUI[Message.Player], MyUI := MaybeMyLevelUpUIPerPlayer[Message.Player]?, SelectedButton := text_button_base[Message.Source]):
            Print("Player selected {SelectedButton.GetText()}")
            PlayerUI.RemoveWidget(MyUI)
            if (set MaybeMyLevelUpUIPerPlayer[Message.Player] = false) {}
            Player:agent = Message.Player

            if(FortChar:fort_character = Player.GetFortCharacter[]):
                FortChar.SetMaxShield(FortChar.GetMaxShield() + 15.0)
            


                
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - hud_manager")
    
    OnEnd<override>():void=
        Print("Nexus OnEnd - hud_manager")