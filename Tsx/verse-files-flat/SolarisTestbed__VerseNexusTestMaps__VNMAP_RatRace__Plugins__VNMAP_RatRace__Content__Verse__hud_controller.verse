using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/UI }
using { /Fortnite.com/UI }
using { /UnrealEngine.com/Temporary/SpatialMath}
using { /Verse.org/Colors/NamedColors }
using { /Verse.org/Colors }
using { /Verse.org/Assets }
using { /Verse.org/Random }

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

# A Verse-authored creative device that can be placed in a level
hud_controller := class(creative_device):
    @editable
    PlayerSpawners : []player_spawner_device = array{}
    @editable
    HUDMsgDevice:hud_message_device = hud_message_device{}

    UIText<localizes>:message = "Rat" # This should localize to "Rat" in the client's chosen language
    var UITextBlock:text_block = text_block{}
    var UIButton:button_regular = button_regular{}
    
    var CycleSpeed:float = 0.0166666667
    var TextureArray:[]texture = array{}

    CyclingActive:logic = true
    CurrentTexture:texture_block = texture_block{DefaultImage := Textures.Tex1, DefaultDesiredSize := vector2{X:= 100.0, Y:= 100.0}}
    var PlayersUI:[player]?canvas = map{}
    
    # Runs when the device is started in a running game
    OnBegin<override>()<suspends> : void =
        Print("Nexus OnBegin - hud_controller")

        set UITextBlock = text_block{DefaultText := UIText}
        UITextBlock.SetTextColor(color{R:=255.0, G:=255.0, B:=255.0})

        set UIButton = button_regular{DefaultText := UIText}

        if(NewArray := TextureArray.Insert[0, array{Textures.Tex1}]):
            set TextureArray = NewArray
        if(NewArray := TextureArray.Insert[0, array{Textures.Tex2}]):
            set TextureArray = NewArray
        if(NewArray := TextureArray.Insert[0, array{Textures.Tex3}]):
            set TextureArray = NewArray

        Print(UIText)

        HUDMsgDevice.SetText(UIText)
        HUDMsgDevice.Show()

        spawn{CycleTextures()}

        for(SpawnerList:PlayerSpawners):
            #Print("texture_device - Subscribing to player spawner!")
            if (Spawner := player_spawner_device[SpawnerList]):
                Spawner.SpawnedEvent.Subscribe(OnPlayerSpawned)

    
    CycleTextures()<suspends>:void=
        loop:
            if(CyclingActive = false):
                break

            NewTextureArray := Shuffle(TextureArray)

            for(TextureList:NewTextureArray, Texture := texture[TextureList]):
                CurrentTexture.SetImage(Texture)
                Sleep(CycleSpeed)
    
    LockTexture():void=
        CurrentTexture.SetImage(Textures.Tex1)

    OnPlayerSpawned(Agent:agent):void=
        #Print("hud_controller - Player Spawned!")
        if (InPlayer := player[Agent], PlayerUI := GetPlayerUI[InPlayer]):
            #Print("hud_controller - 1st if scope")
            if(MyUI := PlayersUI[InPlayer]?):
                PlayerUI.RemoveWidget(MyUI)
            else:
                #Print("hud_controller - else scope")
                MyCanvas : canvas = canvas:
                    Slots := array:
                        canvas_slot: 
                            Anchors := anchors{Minimum := vector2{X := 0.1, Y := 0.5}, Maximum := vector2{X := 0.1, Y := 0.5}}
                            Offsets := margin{Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                            Alignment := vector2{X := 0.5, Y := 0.5}
                            SizeToContent := true
                            Widget := CurrentTexture
                        canvas_slot: 
                            Anchors := anchors{Minimum := vector2{X := 0.1, Y := 0.5}, Maximum := vector2{X := 0.1, Y := 0.5}}
                            Offsets := margin{Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                            Alignment := vector2{X := 0.5, Y := 0.5}
                            SizeToContent := true
                            Widget := UITextBlock
                        canvas_slot: 
                            Anchors := anchors{Minimum := vector2{X := 0.1, Y := 0.8}, Maximum := vector2{X := 0.1, Y := 0.8}}
                            Offsets := margin{Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                            Alignment := vector2{X := 0.5, Y := 0.5}
                            SizeToContent := true
                            Widget := UIButton
                PlayerUI.AddWidget(MyCanvas)
                if(set PlayersUI[InPlayer] = option{MyCanvas}){}
    
    OnEnd<override>():void=
        Print("Nexus OnEnd - hud_controller")