using { /Fortnite.com/Devices }
using { /Fortnite.com/UI}
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
using { /Verse.org/Colors }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /UnrealEngine.com/Temporary/UI }
using { /Verse.org/Simulation/Tags }

collectible_object<public> := class(tag){}

score_manager := class (creative_device):

    var PlayersMap:[player]ui_player = map{}
    @editable
    Collectible_1: collectible_object_device = collectible_object_device {}

    @editable
    var CollectibleValue :int = 50 + 50 - 50 * 2 + 2 + 170000
    @editable
    var PassiveScore : int = ((50 + 50) - (50 * 2) - (100 - 50) + 55 + 5000)
    @editable
    Spawner1: player_spawner_device = player_spawner_device {}
    @editable
    Spawner2: player_spawner_device = player_spawner_device {}
    @editable
    Spawner3: player_spawner_device = player_spawner_device {}
    @editable
    Spawner4: player_spawner_device = player_spawner_device {}
    @editable
    ScoreTimer: timer_device = timer_device{}

    OnEnd<override>():void=
        Print ("Nexus OnEnd - score_manager")
    OnBegin<override>()<suspends>:void =
        Print ("Nexus OnBegin - score_manager")
        SetupCollectibles()
        Spawner1.SpawnedEvent.Subscribe(OnPlayerAdded)
        Spawner2.SpawnedEvent.Subscribe(OnPlayerAdded)
        Spawner3.SpawnedEvent.Subscribe(OnPlayerAdded)
        Spawner4.SpawnedEvent.Subscribe(OnPlayerAdded)
        ScoreTimer.SuccessEvent.Subscribe(OnScoreTimer)

    SetupCollectibles():void=
        AllCollectibles := GetCreativeObjectsWithTag(collectible_object{})

        for (CollectibleItems : AllCollectibles, CollectibleItem := collectible_object_device[CollectibleItems]):
            CollectibleItem.CollectedEvent.Subscribe(OnCollect)
        Print ("Collectibles with tags have been successfully initiliazed")

    OnPlayerAdded(Agent: agent):void=
        if:
            PlayerObj := player[Agent]
        then:
            if(PlayerExists := PlayersMap[PlayerObj]):
                # Doing nothing here since the player exists
            else:
                if:
                    FortChar := Agent.GetFortCharacter[]
                then:

                    CustomPlayer: ui_player = ui_player{Player := Agent}
                    # failable so we use option
                    option:
                        set PlayersMap[PlayerObj] = CustomPlayer
                    CustomPlayer.InitiateUI()
                    
    OnCollect(Agent:agent):void=
        if(PlayerObj:=player [Agent]):
            if(CustomPlayer:ui_player = PlayersMap[PlayerObj]):
                CustomPlayer.AddScore(CollectibleValue)

    OnScoreTimer(Agent:?agent):void=
        for(CustomPlayer : PlayersMap):
            CustomPlayer.AddScore (PassiveScore)
            Print ("All players have successfully gained {PassiveScore}")

        block: