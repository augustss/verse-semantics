using { /Fortnite.com/Devices }
using { /Fortnite.com/UI}
using { /Verse.org/Simulation }
using { /Fortnite.com/Characters }
using { /Verse.org/Colors }
using { /Verse.org/Simulation/Tags }
using { /Verse.org/Random }
using { /Fortnite.com/Game }


spawner<public> := class(tag){}

game_manager := class(creative_device):
    @editable
    PetAssets : []creative_prop_asset = array{}

    # Store the players coming in and out of the game
    var CustomPlayers : [player]onlyrandom_player = map{}

    OnEnd<override>():void=
        Print ("Nexus OnEnd - game_manager")
        
    OnBegin<override>()<suspends>:void=
        Print ("Nexus OnBegin - game_manager")
        GetPlayspace().PlayerRemovedEvent().Subscribe(OnPlayerLeftServer)
        InitSpawners()

# Searching for all the spawners with the tag spawner in the game and looking for spawned events
    InitSpawners():void=
        Spawners := GetCreativeObjectsWithTag(spawner{})
        for (Obj : Spawners):
            if (Spawner := player_spawner_device[Obj]):
                Spawner.SpawnedEvent.Subscribe(OnPlayerSpawned)
                Print ("Successfully init player spawners")
            else:
                Print ("NexusError - game_manager.InitSpawners() | A tagged Spawner was not successfully passed in an array as a player_spawner_device")
# Initializing the player(s) when they spawn 
    OnPlayerSpawned(Agent : agent):void=
        if (_Player := player[Agent]):
            InitPlayer(_Player)
            Print ("Successfully init players spawned")

# Disposing of the Pet if a server leaves and regenerating our map of players
    OnPlayerLeftServer(PlayerLeaving : player):void=
        if(CustomPlayer := CustomPlayers[PlayerLeaving]):
            # Clean up any assets / data from leaving player
            CustomPlayer.Dispose()

            # Remove the player from our map to free up memory
            var NewCustomPlayerMap:[player]onlyrandom_player = map{}
            <# Going through remaining players in map, put them in a new map and replace the old map. 
            We currently cannot remove elements from our map, we have to make a new one #>
            for (Key -> Value : CustomPlayers, Key <> PlayerLeaving):
                set NewCustomPlayerMap = ConcatenateMaps(NewCustomPlayerMap, map{Key => Value})
                Print ("Old playermap replaced, new map has been created")
            set CustomPlayers = NewCustomPlayerMap
        else:
            Print ("NexusError - game_manager.OnPlayerLeftServer(PlayerLeaving : player) | PlayerLeaving not successfully identified to leave from the CustomPlayers Map")
# Initializing the player with a random Pet
    InitPlayer(_Player: player):void =
        Rand := GetRandomInt(0, PetAssets.Length - 1)
        if:
            PetAsset := PetAssets[Rand]
            Agent := agent[_Player]
            Fort := Agent.GetFortCharacter[]
        then:
            if (Existing := CustomPlayers[_Player]){}
            else:
                Fort.EliminatedEvent().Subscribe(OnPlayerEliminated)

                CustomPlayer := onlyrandom_player{Player := _Player, PetAsset := PetAsset}
                CustomPlayer.Init()
                if (set CustomPlayers[_Player] = CustomPlayer) {}
    # Tracking when the player is eliminiated and who eliminated them

    OnPlayerEliminated(Result : elimination_result): void=
        if:
            Killer := Result.EliminatingCharacter?
            Agent := Killer.GetAgent[]
            _Player := player[Agent]
            CustomPlayer := CustomPlayers[_Player]

        then:
            if (not (Result.EliminatedCharacter = Result.EliminatingCharacter?)):
                Print("Player did not eliminate themselves, we're good!")
        else:
            Print ("Nexus Error - game_manager.OnPlayerEliminated(Result : elimination_result) | elimination_result failed to be identified as the Killer")
