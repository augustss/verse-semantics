
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Random }
using { /UnrealEngine.com/Temporary/UI }
using { /Fortnite.com/UI }
using { /Verse.org/Colors }


UI := class(creative_device):

    @editable
    EventsSystem:EventsBus = EventsBus{}

    TextForUI<localizes>(InText : string) : message = "{InText}"

    var MaybeMyUIPerPlayer : [player]?canvas = map{}

    GameStartsText:string = "GAME STARTS IN 3 SECONDS..."
    CrossWinsText:string = "CROSS WON"
    ZeroWinsText:string = "ZERO WON"
    DrawText:string = "DRAW!"
    CrossTurnText:string = "CROSS TURNS"
    ZeroTurnText:string = "ZERO TURNS"

    var GameMenuUI:canvas = canvas{}
    var OverlayLaybelWidget:widget = overlay{}
    


    OnBegin<override>()<suspends>:void=
        Print("UI INIT")
        FindPlayerAndInitUI()
        spawn{GameFinishAwaiter()}
        spawn{FigureTurnAwaiter()}
    
    
    FindPlayerAndInitUI():void=
        if (P := GetPlayspace().GetPlayers()[0], PlayerUI := GetPlayerUI[P]):
            set GameMenuUI = CreateMainUI()
            PlayerUI.AddWidget(GameMenuUI)


    CreateMainUI():canvas =        
        MainCanvas:canvas = canvas:
            Slots := array:
                canvas_slot: # Upper Left of Screen
                    Anchors := anchors{Minimum := vector2{X := 0.0, Y := 0.0}, Maximum := vector2{X := 0.0, Y := 0.0}}
                    Offsets := margin{Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                    Alignment := vector2{X := 0.0, Y := 0.0}
                    SizeToContent := true
                    Widget := MakeOverlayLaybelWidget(TextForUI(GameStartsText))
        return MainCanvas


    MakeOverlayLaybelWidget(InputMessage:message):widget=
        set OverlayLaybelWidget = overlay:
            Slots := array:
                overlay_slot:
                    Widget := color_block:
                        DefaultColor := NamedColors.Aqua
                        DefaultOpacity := 1.0
                        DefaultDesiredSize := vector2{X := 600.0, Y := 60.0}                        
                overlay_slot:
                    Widget := text_block:
                        DefaultText := InputMessage


    MakeCanvasSlotWithWidget(InputWidget:widget):canvas_slot=
        canvas_slot: # Upper Left of Screen
            Anchors := anchors{Minimum := vector2{X := 0.0, Y := 0.0}, Maximum := vector2{X := 0.0, Y := 0.0}}
            Offsets := margin{Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
            Alignment := vector2{X := 0.0, Y := 0.0}
            SizeToContent := true
            Widget := InputWidget


    ChangeOverlayLaybel(InputText:string):void=
        GameMenuUI.RemoveWidget(OverlayLaybelWidget)
        set OverlayLaybelWidget = MakeOverlayLaybelWidget(TextForUI(InputText))
        GameMenuUI.AddWidget(
            MakeCanvasSlotWithWidget(OverlayLaybelWidget)
            )

        # if (SW:=GameMenuUI.Slots[0].Widget):
        #     GameMenuUI.RemoveWidget(OverlayLaybelWidget)
        #     if (var Slot:canvas_slot = GameMenuUI.Slots[0]) 
        #     then set Slot = MakeCanvasSlotWithWidget(OverlayWidget)
        #     else Print("Problem to find slot to change overlay")
            
            # GameMenuUI.AddWidget(
            #     MakeCanvasSlotWithWidget(OverlayWidget)
            #     )


    GameFinishAwaiter()<suspends>:void=
        Winner:int = EventsSystem.GameFinishEvent.Await()
        ProcessResult(Winner)

    
    FigureTurnAwaiter()<suspends>:void=
        Print("Before Await")
        Figure:int = EventsSystem.FigureTurnEvent.Await()
        Print("After Await")
        
        case(Figure):
            1 => ChangeOverlayLaybel(CrossTurnText)
            0 => ChangeOverlayLaybel(ZeroTurnText)
            _ =>

        spawn{FigureTurnAwaiter()}

    
    ProcessResult(InputWinner:int)<suspends>:void=
        case(InputWinner):
            1 => 
                ChangeOverlayLaybel(CrossWinsText) 
            0 => 
                ChangeOverlayLaybel(ZeroWinsText)
            _ => 
                ChangeOverlayLaybel(DrawText)
        