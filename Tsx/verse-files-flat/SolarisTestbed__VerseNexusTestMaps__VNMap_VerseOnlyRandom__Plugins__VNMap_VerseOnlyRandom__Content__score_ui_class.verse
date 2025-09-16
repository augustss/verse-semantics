using { /Fortnite.com/Devices }
using { /Fortnite.com/UI}
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
using { /Verse.org/Colors }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /UnrealEngine.com/Temporary/UI }
using { /Verse.org/Simulation/Tags }

ui_player := class<unique>():
    StringToMessage<localizes>(value:string)<computes> : message = "{value}"

    Player: agent

    var ScoreText : text_block = text_block{}
    var Score: int = 0

    InitiateUI():void=
        set ScoreText = text_block{DefaultText := StringToMessage("Score: {Score}"), DefaultTextColor:= NamedColors.White, DefaultShadowColor:=NamedColors.Black}
        # Creating the score widget canvas
        PlayerScoreWidget:canvas = CreateUserScoreUI(ScoreText)
        # Grabbing the player and UI object
        if (PlayerObj:=player[Player], PlayerUI := GetPlayerUI[PlayerObj]):
            PlayerUI.AddWidget(PlayerScoreWidget)
            Print ("Score UI successfully created for player(s)")

    # Creating a canvas and assigning its location. Using overlay slots for better visibility
    CreateUserScoreUI(TheScoreText:text_block):canvas=
        UICanvas : canvas = canvas:
            Slots := array:
                canvas_slot:
                    Anchors := anchors{Minimum := vector2{X := 0.5, Y := 0.2}, Maximum := vector2{X := 0.5, Y:= 0.2} }
                    Offsets := margin {Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                    Alignment := vector2{X := 0.5, Y := 0.0}
                    SizeToContent := true
                    Widget := overlay:
                        Slots := array:
                            overlay_slot:
                                Widget := color_block:
                                    DefaultColor := NamedColors.Black
                                    DefaultOpacity := 0.1
                                    DefaultDesiredSize := vector2{X := 1024.0, Y := 128.0} 
                            overlay_slot:
                                Widget := ScoreText
        return UICanvas
# Incrementing our Score to the passed in value and updating the UI with UpdateScoreUI()
    AddScore(Amount:int): void =
        set Score += Amount
        UpdateScoreUI()
# Sets our string to what the new value of the score is
    UpdateScoreUI():void=
        ScoreText.SetText(StringToMessage("Score: {Score}"))
