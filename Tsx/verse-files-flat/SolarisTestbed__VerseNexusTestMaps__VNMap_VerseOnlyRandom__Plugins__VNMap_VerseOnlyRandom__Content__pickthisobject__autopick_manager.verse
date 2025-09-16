using { /Fortnite.com/Devices }
using { /Fortnite.com/UI}
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
using { /Verse.org/Colors }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /UnrealEngine.com/Temporary/UI }
using { /Verse.org/Simulation/Tags }
using { /Verse.org/Random}


autopick_manager := class(creative_device):

    var Cakes : [cake]cake = map {}

    var AutoPickerPlayerMap:[player]autopicker_player = map{}

    @editable
    CakePropAsset : creative_prop_asset = DefaultCreativePropAsset
    @editable
    Spawner1: player_spawner_device = player_spawner_device {}
    @editable
    Spawner2: player_spawner_device = player_spawner_device {}
    @editable
    Spawner3: player_spawner_device = player_spawner_device {}
    @editable
    Spawner4: player_spawner_device = player_spawner_device {}
    @editable
    CakeCollectionSound: audio_player_device = audio_player_device {}
    @editable
    CakeCollectionVFX: vfx_spawner_device = vfx_spawner_device {}

    OnBegin<override>()<suspends>:void=
        Print ("Nexus OnBegin - autopick_manager")
        Spawner1.SpawnedEvent.Subscribe(OnCakePlayerAdded)
        Spawner2.SpawnedEvent.Subscribe(OnCakePlayerAdded)
        Spawner3.SpawnedEvent.Subscribe(OnCakePlayerAdded)
        Spawner4.SpawnedEvent.Subscribe(OnCakePlayerAdded)
        
        # PreParePlayersForCakes()

    OnEnd<override>():void=
        Print ("Nexus OnEnd - autopick_manager")
            

    PreParePlayersForCakes():void=
        for(Player: GetPlayspace().GetPlayers()):
            # AllPlayers := GetPlayspace().GetPlayers()
            if:
                # Player := AllPlayers
                Players := Player
                Agents := agent[Players]
                Fort := Agents.GetFortCharacter[]
                FortCharacter := Agents.GetFortCharacter[()]
            then:
                TaggedCakes := GetCreativeObjectsWithTag(tag_cakes{})
                for (Object : TaggedCakes, Prop := creative_prop[Object]):
                    Cake := cake{cakepropasset := CakePropAsset, cakeprop := Prop, OldTransform := Prop.GetTransform(), FortCharacter := FortCharacter, OnCakeCollected := PreparedCake, AutoPickerDevice := Self}
                    Cake.InitCakes()
                    if ( set Cakes[Cake] = Cake) {}
                
    PreparedCake(Cake: cake):void=
        block:
        # spawn:
        #     OnCakeCollected()
    OnCakePlayerAdded(Agent : agent):void=
        if:
            PlayerObj := player[Agent]
        then:
            if(PlayerExists := AutoPickerPlayerMap[PlayerObj]):
                
                # Doing nothing here since the player exists
            else:
                if:
                    FortChar := Agent.GetFortCharacter[]
                then:

                    AutoPickerPlayer: autopicker_player = autopicker_player{Player := Agent}
                    # failable so we use option
                    option:
                        set AutoPickerPlayerMap[PlayerObj] = AutoPickerPlayer

                    AutoPickerPlayer.InitiateCakesUI()

                    TaggedCakes := GetCreativeObjectsWithTag(tag_cakes{})
                    for (Object : TaggedCakes, Prop := creative_prop[Object]):
                        Cake := cake{cakepropasset := CakePropAsset, cakeprop := Prop, OldTransform := Prop.GetTransform(), FortCharacter := FortChar, OnCakeCollected := PreparedCake, AutoPickerDevice := Self}
                        Cake.InitCakes()
                        if ( set Cakes[Cake] = Cake) {}
                    Print("Succesfully Init Cakes UI and Cakes")

                else:
                    Print ("NexusError - autopick_manager.OnCakePlayerAdded(Agent : agent)  | Player not successfully added to AutoPickerPlayerMap ")
        else:
            Print ("NexusError -  autopick_manager.OnCakePlayerAdded(Agent : agent) | Agent not successfully added ")
    
                    
    OnCakeCollected(Agent : agent):void=
        if(PlayerObj:=player [Agent]):
            if(MyAutoPlayer:autopicker_player = AutoPickerPlayerMap[PlayerObj]):
                MyAutoPlayer.OnCakeCollected()
                CakeCollectionSound.Play(Agent)
                CakeCollectionVFX.Enable()


    
    