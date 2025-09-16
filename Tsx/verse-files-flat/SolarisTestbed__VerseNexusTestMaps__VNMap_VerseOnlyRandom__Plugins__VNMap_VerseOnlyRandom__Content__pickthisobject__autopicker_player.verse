using { /Fortnite.com/Devices }
using { /Fortnite.com/UI}
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
using { /Verse.org/Colors }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /UnrealEngine.com/Temporary/UI }

autopicker_player := class:

    # We use this function to convert strings to a message
    StringToMessage<localizes>(value:string)<computes> : message = "{value}"
    var Cakes : int = 0
    var CakesText : text_block = text_block {}

    Player : agent

    GetCakePropCount():int=
        Cakes
    OnCakeCollected():void=
        set Cakes += 1
        UpdateCakesUI()

    
    InitiateCakesUI():void=
        set CakesText = text_block{DefaultText := StringToMessage("Cakes: {Cakes}"), 
        DefaultTextColor:= NamedColors.Pink, 
        DefaultShadowColor:=NamedColors.Black}
        # Creating the score widget canvas
        PlayerCakesWidget:canvas = CreateCakesUI(CakesText)
        # Grabbing the player and UI object
        if (PlayerObj:=player[Player], PlayerUI := GetPlayerUI[PlayerObj]):
            PlayerUI.AddWidget(PlayerCakesWidget)
            Print ("Create Cakes UI successfully created for player(s)")

# Creating our cakes ui by making our canvas and applying a color_block widget with a text_block widget for the counter
    CreateCakesUI(TheCakeText: text_block):canvas=
        CakeCanvas : canvas = canvas:
            Slots := array:
                canvas_slot:
                    Anchors := anchors{Minimum := vector2{X := 0.25, Y := 0.3}, Maximum := vector2{X := 0.25, Y:= 0.3} }
                    Offsets := margin {Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                    Alignment := vector2{X := 0.5, Y := -0.5}
                    SizeToContent := true
                    Widget := overlay:
                        Slots := array:
                            overlay_slot:
                                Widget := color_block:
                                    DefaultColor := NamedColors.Pink
                                    DefaultOpacity := 0.3
                                    DefaultDesiredSize := vector2{X := 720.0, Y := 128.0} 
                            overlay_slot:
                                Widget := CakesText
        return CakeCanvas


# Setting our cakes text to the new value of what it is
    UpdateCakesUI():void=
        CakesText.SetText(StringToMessage("Cakes: {Cakes}"))
