using { /Fortnite.com/FortPlayerUtilities }
using { /Fortnite.com/Characters }
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation/Tags }
using { /Verse.org/Colors }

respawn_manager:= class (creative_device):
    @editable
    var SpawnPos : vector3 = vector3 {}
    @editable
    var SpawnRot : rotation = rotation {}
    @editable
    var RespawnTimer : float = 3.0
    @editable
    RespawnZone: mutator_zone_device = mutator_zone_device {}
    @editable
    StartZone: mutator_zone_device = mutator_zone_device {}
    @editable
    BombProp: creative_prop = creative_prop {}
    @editable
    Bomb: explosive_device = explosive_device {}
    @editable
    RaceDevice_1: race_checkpoint_device = race_checkpoint_device {}
    @editable
    RaceDevice_2: race_checkpoint_device = race_checkpoint_device {}
    @editable
    RaceDevice_3: race_checkpoint_device = race_checkpoint_device {}
    @editable
    RaceDevice_4: race_checkpoint_device = race_checkpoint_device {}
    @editable
    RaceDevice_5: race_checkpoint_device = race_checkpoint_device {}
    @editable
    RaceManager: race_manager_device = race_manager_device {}
    @editable
    EndGameDevice: end_game_device = end_game_device {}
    @editable
    respawnaudiodevice: audio_player_device = audio_player_device {}
    @editable
    explosionaudiodevice: audio_player_device = audio_player_device {}



    var LastPosition : vector3 = vector3{}
    var CurrentPosition : vector3 = vector3{}
    var Speed : float = 0.0
    var PlayerIsAlive : logic = true
    @editable
    var MinSpeed : float = 0.00005
    

    BombFollowPlayer(FortCharacter: fort_character):void=
        if (BombProp.TeleportTo[FortCharacter.GetTransform().Translation, rotation{}]) {}

    StartMoving (Agent: agent):void=
        Print("Get Moving, you've got 3 seconds!")
        set PlayerIsAlive = true
        StartZone.Disable()
        StartRace(Agent)
        Print ("Race device is enabled")
        spawn:
            ElimIfBelowSpeed(Agent)

    ElimIfBelowSpeed(Agent: agent)<suspends>:void=
        Sleep(3.0)
        loop:
            if (PlayerIsAlive = false):
                Bomb.Reset()
                break
            if (FortCharacter := Agent.GetFortCharacter[]):
                set Speed = GetSpeed(FortCharacter)
                BombFollowPlayer(FortCharacter)
                Print ("Your speed is {Speed} Km / Hour")
                if (Speed < MinSpeed):
                    Bomb.Explode(Agent)
                    explosionaudiodevice.Play(Agent)
                    Bomb.Reset()
                    StartZone.Enable()
                    Print ("You have died")
                    set PlayerIsAlive = false


    GetSpeed (FortCharacter : fort_character)<suspends>:float=
        set LastPosition = CurrentPosition
        Sleep(0.2)
        set CurrentPosition = FortCharacter.GetTransform().Translation

        PlayerDistance := DistanceXY(CurrentPosition, LastPosition)
        # Speed is Distance / Time. This gives us centimeters/second. Fortnite tracks speed using Kilometers so we need to convert that product
        SpeedKMH := 3.6 * ((PlayerDistance / 0.2) / 100.0)

        return SpeedKMH

    OnBegin<override>()<suspends>:void=
        Print ("======Nexus OnBegin respawn_manager======")
        RespawnZone.AgentEntersEvent.Subscribe(RespawnPlayer)
        StartZone.AgentEntersEvent.Subscribe(StartMoving)
        StartZone.AgentEntersEvent.Subscribe(StartRace)
        RaceDevice_5.CheckpointCompletedEvent.Subscribe(EndTheGame)

    StartRace(Agent: agent):void=
        RaceManager.Begin()
        Print ("Race has started")


    EndTheGame(Agent: agent):void=
            EndGameDevice.Activate(Agent)
            RaceManager.End()
            Print ("Ending the game")

    OnEnd<override>():void=
        Print ("======Nexus OnEnd respawn_manager======")


# Logic for loop respawning the player
    # LoopRespawn()<suspends>:void=
    #     loop:
    #         Print("Player will respawn in 3 seconds")
    #         Sleep(3.0)
    #         Players := Self.GetPlayspace().GetPlayers()
    #         for(SomePlayer:Players):
    #             PlayerAgent:agent = SomePlayer
    #             RespawnPlayer(SomePlayer)

    RespawnPlayer(Agent : agent): void=
        if (Player := player[Agent]):
            Print ("Player successfully valid")
            Player.Respawn(SpawnPos, SpawnRot)
            respawnaudiodevice.Play(Agent)
            Print ("Player successfully respawned")
        # AllPlayers := GetPlayspace().GetPlayers()
        # if (Player := AllPlayers[0], AgentToHide := agent[Player], FirstFortChar := AgentToHide.GetFortCharacter[]):
        #     Print ("Player successfully respawned")
