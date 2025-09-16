# Copyright Epic Games, Inc. All Rights Reserved.

using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }

verse_persistence_game_device<public> := class(creative_device):

    @editable
    PersistentPlayerDataManager:persistent_player_data_manager = persistent_player_data_manager{}

    @editable
    PlayerStatLeaderboardManager:player_stat_leaderboard_manager_device = player_stat_leaderboard_manager_device{}

    @editable
    LeaderboardWriterDevice:leaderboard_writer_device = leaderboard_writer_device{}

    @editable
    Team1RotatingLeaderboardBillboard:billboard_device = billboard_device{}

    @editable
    Team1PlayerEliminationsLeaderboardBillboard:billboard_device = billboard_device{}

    @editable
    Team1TeamEliminationsLeaderboardBillboard:billboard_device = billboard_device{}

    @editable
    Team2RotatingLeaderboardBillboard:billboard_device = billboard_device{}

    @editable
    Team2PlayerEliminationsLeaderboardBillboard:billboard_device = billboard_device{}

    @editable
    Team2TeamEliminationsLeaderboardBillboard:billboard_device = billboard_device{}

    @editable
    Team1SelectActorVolume:volume_device = volume_device{}

    @editable
    Team2SelectActorVolume:volume_device = volume_device{}

    var Team1LeaderboardViewerAgent:?agent = false
    var Team2LeaderboardViewerAgent:?agent = false

    OnBegin<override>():void =
        Team1SelectActorVolume.AgentEntersEvent.Subscribe(HandleSelectActorTeam1)
        Team1SelectActorVolume.AgentExitsEvent.Subscribe(HandleClearActorTeam1)

        Team2SelectActorVolume.AgentEntersEvent.Subscribe(HandleSelectActorTeam2)
        Team2SelectActorVolume.AgentExitsEvent.Subscribe(HandleClearActorTeam2)
        

        Self.GetPlayspace().PlayerAddedEvent().Subscribe(PersistentPlayerDataManager.InitializePlayer)
        for (Player : Self.GetPlayspace().GetPlayers()):
            PersistentPlayerDataManager.InitializePlayer(Player)
            UpdateLeaderboard()
            UpdateLeaderboard()
            spawn{ RunLeaderboardUpdateLoop(Player) }

    HandleSelectActorTeam1(Agent:agent):void= 
        set Team1LeaderboardViewerAgent = option{Agent}
        UpdateLeaderboard()

    HandleClearActorTeam1(Agent:agent):void= 
        if (Agent = Team1LeaderboardViewerAgent ):
            set Team1LeaderboardViewerAgent = false
            UpdateLeaderboard()

    HandleSelectActorTeam2(Agent:agent):void= 
        set Team2LeaderboardViewerAgent = option{Agent}
        UpdateLeaderboard()

    HandleClearActorTeam2(Agent:agent):void= 
        if (Agent = Team2LeaderboardViewerAgent ):
            set Team2LeaderboardViewerAgent = false
            UpdateLeaderboard()

    RunLeaderboardUpdateLoop(Agent:agent)<suspends>:void =
        block:
            loop:
                PlayerStatLeaderboardManager.ActivateNextLeaderboard()
                UpdateLeaderboard()
                Sleep(5.0)
            loop:
                UpdateLeaderboard()
                Sleep(0.2)
    
    UpdateLeaderboard():void= 
        RotatingLeaderboardInfo1:player_leaderboard_info = PlayerStatLeaderboardManager.GetCurrentLeaderboardInfo(Team1LeaderboardViewerAgent)
        RotatingLeaderboardMessage1:message = LeaderboardWriterDevice.GetLeaderboardMessage(RotatingLeaderboardInfo1)
        Team1RotatingLeaderboardBillboard.SetText(RotatingLeaderboardMessage1)

        RotatingLeaderboardInfo2:player_leaderboard_info = PlayerStatLeaderboardManager.GetCurrentLeaderboardInfo(Team2LeaderboardViewerAgent)
        RotatingLeaderboardMessage2:message = LeaderboardWriterDevice.GetLeaderboardMessage(RotatingLeaderboardInfo2)
        Team2RotatingLeaderboardBillboard.SetText(RotatingLeaderboardMessage2)

        PlayerEliminationsLeaderboardInfo1:player_leaderboard_info = PlayerStatLeaderboardManager.GetPlayerEliminationsLeaderboardInfo(Team1LeaderboardViewerAgent)
        PlayerEliminationsLeaderboardMessage1:message = LeaderboardWriterDevice.GetLeaderboardMessage(PlayerEliminationsLeaderboardInfo1)
        Team1PlayerEliminationsLeaderboardBillboard.SetText(PlayerEliminationsLeaderboardMessage1)

        PlayerEliminationsLeaderboardInfo2:player_leaderboard_info = PlayerStatLeaderboardManager.GetPlayerEliminationsLeaderboardInfo(Team2LeaderboardViewerAgent)
        PlayerEliminationsLeaderboardMessage2:message = LeaderboardWriterDevice.GetLeaderboardMessage(PlayerEliminationsLeaderboardInfo2)
        Team2PlayerEliminationsLeaderboardBillboard.SetText(PlayerEliminationsLeaderboardMessage2)

        TeamEliminationsLeaderboardInfo1:player_leaderboard_info = PlayerStatLeaderboardManager.GetTeamEliminationsLeaderboardInfo(Team1LeaderboardViewerAgent)
        TeamEliminationsLeaderboardMessage1:message = LeaderboardWriterDevice.GetLeaderboardMessage(TeamEliminationsLeaderboardInfo1)
        Team1TeamEliminationsLeaderboardBillboard.SetText(TeamEliminationsLeaderboardMessage1)

        TeamEliminationsLeaderboardInfo2:player_leaderboard_info = PlayerStatLeaderboardManager.GetTeamEliminationsLeaderboardInfo(Team2LeaderboardViewerAgent)
        TeamEliminationsLeaderboardMessage2:message = LeaderboardWriterDevice.GetLeaderboardMessage(TeamEliminationsLeaderboardInfo2)
        Team2TeamEliminationsLeaderboardBillboard.SetText(TeamEliminationsLeaderboardMessage2)