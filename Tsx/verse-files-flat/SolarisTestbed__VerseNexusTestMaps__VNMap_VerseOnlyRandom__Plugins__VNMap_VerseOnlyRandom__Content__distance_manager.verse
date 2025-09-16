
using { /Fortnite.com/Devices }
using { /Fortnite.com/UI}
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
using { /Verse.org/Colors }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /UnrealEngine.com/Temporary/UI }

distance_manager := class(creative_device):

    UIText<localizes>(Value : string) : message = "{Value}"
        # Function written this way so i can reference this OnBegin
        (Player : player).InitUI() : void =

            TextWidget : text_block = text_block{DefaultTextColor := NamedColors.LightGreen}
            NewCanvas : canvas = canvas:
                Slots := array:
                    canvas_slot:
                        Widget := TextWidget
                        Anchors := anchors{Minimum := vector2{ X:= 0.6, Y := 0.6 }, Maximum:= vector2{ X:= 0.6, Y := 0.6 }}
            if (PlayerUI := GetPlayerUI[Player], FortCharacter := Player.GetFortCharacter[]):
                PlayerUI.AddWidget(NewCanvas)
                spawn:
                    FortCharacter.UpdateDistance(TextWidget)
        (FortCharacter : fort_character).UpdateDistance( TextBlock : text_block)<suspends> : void =
            loop:
                # How smooth we want our text to update
                Sleep(0.2)
                # Getting our FortCharacters Z location. Then dividing by 100 because we want this in Meters not CentiMeters.
                DistanceVerticalMeters := FortCharacter.GetTransform().Translation.Z / 100.0
                # Now we round that using the Floor function to give us a rounded number instead of decimals
                if (RoundedDistance := Floor[DistanceVerticalMeters] - 4):
                    TextBlock.SetText(UIText("- {RoundedDistance} m-"))
    #Checking for new players that join the game mid session
    HandlePlayerJoin(Agent : agent) : void =
        if (Player:= player[Agent]):
            Player.InitUI()
            
    OnEnd<override>():void=
        Print ("Nexus OnEnd - distance_manager")
    OnBegin<override>()<suspends>:void=
        Print ("Nexus OnBegin - distance_manager")
        GetPlayspace().PlayerAddedEvent().Subscribe(HandlePlayerJoin)
        # Runs the code on begin for all players
        for(Player: GetPlayspace().GetPlayers()):
            Player.InitUI()