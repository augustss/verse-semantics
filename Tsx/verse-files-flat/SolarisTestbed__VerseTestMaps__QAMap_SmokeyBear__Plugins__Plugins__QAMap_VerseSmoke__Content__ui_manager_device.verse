using { /Fortnite.com/FortPlayerUtilities }
using { /Fortnite.com/Characters }
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation/Tags }
using { /UnrealEngine.com/Temporary/UI }
using { /Fortnite.com/UI}
using { /Verse.org/Verse }
using { /Verse.org/Colors }

ui_manager_device:= class(creative_device):

    @editable
    var UIButton : button_device = button_device {}

    @editable
    var AssignAllHUDSTrigger : trigger_device = trigger_device {}

    @editable
    var RemoveAllHUDSTrigger : trigger_device = trigger_device {}

    @editable
    var SetBillboardTextOnBegin : billboard_device = billboard_device {}

    @editable
    BillBoardMessageText : string = "Test"

    var MaybeMyUIPerPlayer : [player]?canvas = map {}

    MessageToSet<localizes>(String: string) : message = 
        "{String}"
    

    OnEnd<override>():void=
        Print("OnEnd ui_manager_device")
    OnBegin<override>()<suspends>:void=
        Print("OnBegin ui_manager_device")
        
        UIButton.InteractedWithEvent.Subscribe(HandleMyUI)
        AssignAllHUDSTrigger.TriggeredEvent.Subscribe(AssignAllHUDS)
        RemoveAllHUDSTrigger.TriggeredEvent.Subscribe(RemoveAllHUDS)
        SetBillboardTextOnBegin.SetText(MessageToSet(BillBoardMessageText))
  

    AssignAllHUDS(Agent: ?agent):void=

        CurrentPlayers:= GetPlayspace().GetPlayers()
        for (Player:CurrentPlayers):
            if(MyAgent:= agent[Player]):
                Print ("[SUCCESS] Checking if we need to assign HUDs to all Players | AssignAllHUDS()", ?Color:=NamedColors.LightGreen)
                HandleMyUI(MyAgent)
            else:
                Print ("[FAILURE] All Players were not checked properly | AssignAllHUDS()", ?Color:=NamedColors.Red)

    HandleMyUI(Agent:agent):void=
        if (Player:= player[Agent], PlayerUI:= GetPlayerUI[Player]):
            if (MyUI:= MaybeMyUIPerPlayer[Player]?):
                Print("Player already has a HUD")
                PlayerUI.RemoveWidget(MyUI)
                if (set MaybeMyUIPerPlayer[Player] = false):
                    Print("[SUCCESS] We don't need a new HUD | HandleMyUI()", ?Color:=NamedColors.LightGreen)
                else:
                    Print("[FAILURE] Something failed here | HandleMyUI()", ?Color:=NamedColors.Red)
            
            else:
                Print ("Player does not have a HUD")
                var MyCanvas : canvas = MakeCanvas()
                PlayerUI.AddWidget(MyCanvas, player_ui_slot{InputMode:= ui_input_mode.All})
                if (set MaybeMyUIPerPlayer[Player] = option{MyCanvas}):
                    Print("[SUCCESS] The player has been given a HUD | HandleMyUI()", ?Color:=NamedColors.LightGreen)
                else:
                    Print ("[FAILURE] HUD was not given to a Player | HandleMyUI()", ?Color:=NamedColors.Red)


    @editable
    Button1_Text : string = " "

    ButtonIsPressed : logic = false
    
    @editable
    Button1_Position : vector2 = vector2 {}

    @editable
    Button1_Size : vector2 = vector2 {}

    @editable
    Button2_Text : string = " "
    @editable
    Button2_Position : vector2 = vector2 {}
    @editable
    Button2_Size : vector2 = vector2 {}

    StringToMessage<localizes>(String:string):message="{String}"

    MakeCanvas():canvas=

        Button1 : button_regular = button_regular {}

        Button1.SetText(StringToMessage(Button1_Text))

        Button1.OnClick().Subscribe(HandleButton1)

        Button2 : button_loud = button_loud {}

        Button2.SetText(StringToMessage(Button2_Text))

        Button2.OnClick().Subscribe(HandleButton2)

        var MyCanvas : canvas = canvas:
            Slots:= array:
                canvas_slot:
                    Widget:= Button1
                    ZOrder:= 0
                    SizeToContent:= true
                    Offsets:= margin {Top:= Button1_Position.Y, Left:= Button1_Position.X, Bottom:= Button1_Size.Y, Right:= Button1_Size.X}
                canvas_slot:
                    Widget:= Button2
                    ZOrder:= 1
                    SizeToContent:= true
                    Offsets:= margin {Top:= Button2_Position.Y, Left:= Button2_Position.X, Bottom:= Button2_Size.Y, Right:= Button2_Size.X}

        return MyCanvas

    @editable 
    Button1_Outcome_Trigger :trigger_device = trigger_device {}

    @editable 
    Button2_Outcome_Trigger :trigger_device = trigger_device {}

    HandleButton1(Wdata:widget_message):void=
            Print ("Button 1 Pressed")
            RemoveHUD(Wdata)
            if (Agent:= agent[Wdata.Player]):
                Button1_Outcome_Trigger.Trigger(Agent)
        else:
            Print ("Slow down cowboy")

    HandleButton2(Wdata:widget_message):void=
        Print ("Button 2 Pressed")
        RemoveHUD(Wdata)
        if (Agent:= agent[Wdata.Player]):
            Button2_Outcome_Trigger.Trigger(Agent)

    RemoveAllHUDS(Agent: ?agent):void=
        Print ("Removing all HUDs")
        CurrentPlayers:= GetPlayspace().GetPlayers()
        for (Player: CurrentPlayers):
            if (MyAgent:= agent[Player]):
                if (PlayerUI:= GetPlayerUI[Player], MyUI:= MaybeMyUIPerPlayer[Player]?):
                    PlayerUI.RemoveWidget(MyUI)
                    Print("[SUCCESS] All HUDs were successfully removed | RemoveAllHUDS()", ?Color:=NamedColors.LightGreen)
                    if(set MaybeMyUIPerPlayer[Player] = false) {}
                else:
                    Print("[NOT A FAILURE] Some HUDs were not successfully removed because none were available to remove | RemoveAllHUDS()", ?Color:=NamedColors.Orange)


    RemoveHUD(Wdata:widget_message):void=
        Print ("Removing HUD")
        if (Player:= Wdata.Player, PlayerUI:= GetPlayerUI[Player], SelectedButton:= Wdata.Source):
            if (MyUI:= MaybeMyUIPerPlayer[Player]?):
                PlayerUI.RemoveWidget(MyUI)
                Print("[SUCCESS] HUD was successfully removed | RemoveHUD()", ?Color:=NamedColors.LightGreen)
                if (set MaybeMyUIPerPlayer[Player] = false) {}
            else:
                Print("[NOT A FAILURE] A HUD was not successfully removed because none were available  to remove | RemoveHUD()", ?Color:=NamedColors.Orange)
