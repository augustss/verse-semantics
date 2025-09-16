
using { /Fortnite.com/Devices }
using { /Fortnite.com/UI}
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
using { /Verse.org/Colors }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /UnrealEngine.com/Temporary/UI }
using { /Verse.org/Simulation/Tags }


teamdeath_player := class<unique>():

    # For our Team DeathMatch stuff
    MyAgentObj : agent


    var Team1TextBlock : text_block = text_block {}
    var Team2TextBlock : text_block = text_block {}

    var TeamIndex : int = 0
    var Team1Points : int = 0
    var Team2Points : int = 0

    var CurrentElims<private> : int = 0
    var CurrentDeaths<private> : int = 0

    # Setter for current Death Counts
    IncDeathCount(): void=
        set CurrentDeaths += 1
    # Getter for current Death Counts
    GetDeathCount()<transacts>:int=
        return CurrentDeaths
    # Getter for Current Eliminations
    AwardElimination(): void=
        set CurrentElims += 1
    # Setter for Current Eliminations
    GetElimCount()<transacts>:int=
        return CurrentElims

    StringToMessage<localizes>(value:string)<computes> : message = "{value}"

    # Initializing our players with the Team 1 and Team 2 text blocks on their UI
    InitPlayer():void=
                                                                                        set Team1TextBlock = text_block{DefaultText := StringToMessage("Team 1: {Team1Points}"),
                                                                                        DefaultTextColor:=NamedColors.LightBlue, 
                                                                                        DefaultShadowColor:=NamedColors.Black, 
                                                                                        DefaultShadowOffset:=option{vector2{X:=2.0, Y:=2.0}}}
                                                                                        Team1TextBlock.SetShadowOpacity(1.0)

                                                                                        set Team2TextBlock = text_block{DefaultText := StringToMessage("Team 2: {Team2Points}"), 
                                                                                        DefaultTextColor:=NamedColors.Orange, 
                                                                                        DefaultShadowColor:=NamedColors.Black, 
                                                                                        DefaultShadowOffset:=option{vector2{X:=2.0, Y:=2.0}}}
                                                                                        Team2TextBlock.SetShadowOpacity(1.0)


                                                                                        PlayerScoreWidget:canvas = CreateUserUI()
                                                                                        if(PlayerObj:=player[MyAgentObj], PlayerUI := GetPlayerUI[PlayerObj]):
                                                                                            PlayerUI.AddWidget(PlayerScoreWidget)


    SetTeam(Team:int):void=
                                                                                        set TeamIndex = Team

    GetTeam():int=
                                                                                        return TeamIndex
# We update the UIScore for each team
    UpdateScore(T1Score:int, T2Score:int):void=
                                                                                        set Team1Points = T1Score
                                                                                        set Team2Points = T2Score
                                                                                        Team1TextBlock.SetText(StringToMessage("Team 1: {Team1Points}"))
                                                                                        Team2TextBlock.SetText(StringToMessage("Team 2: {Team2Points}"))
# Creating our canvas and placing our team text_block widgets
    CreateUserUI<private>() : canvas=
        UICanvas : canvas = canvas:
            Slots := array:
                canvas_slot: 
                    Anchors := anchors{Minimum := vector2{X := 0.01, Y := 0.5}, Maximum := vector2{X := 0.01, Y := 0.5}}
                    Offsets := margin{Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                    Alignment := vector2{X := -3.5, Y := 7.0}
                    SizeToContent := true
                    ZOrder := 30
                    Widget := overlay:
                        Slots := array:                              
                            overlay_slot:
                                Widget := Team1TextBlock


                canvas_slot: 
                    Anchors := anchors{Minimum := vector2{X := 0.01, Y := 0.50}, Maximum := vector2{X := 0.01, Y := 0.50}}
                    Offsets := margin{Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                    Alignment := vector2{X := -3.15, Y := 6.0}
                    SizeToContent := true
                    ZOrder := 30
                    Widget := overlay:
                        Slots := array:                              
                            overlay_slot:
                                Widget := Team2TextBlock