# Copyright Epic Games, Inc. All Rights Reserved.

using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }

player_leaderboard_stat_type := enum{
    Current,
    StatMin,
    StatMax,
    Total
}

leaderboard_type := enum{
    Player,
    Team
}

sort_order := enum{
    Ascending,
    Descending
}

EntryPlayerMessage<localizes>(
    Rank:int,
    Agent:agent,
    PrimaryScore:int,
    SecondaryScore:int
    ):message = "{Rank}. {Agent}: {PrimaryScore} ({SecondaryScore})"

EntryTeamMessage<localizes>(
    Rank:int,
    Name:string,
    PrimaryScore:int,
    SecondaryScore:int
    ):message = "{Rank}. {Name}: {PrimaryScore} ({SecondaryScore})"

player_leaderboard_entry := class:
    Player:?player = false
    Rank:int = 0
    Name:string = ""
    PrimaryScore:int
    SecondaryScore:int

MakePlayerLeaderboardEntry<constructor>(Src:player_leaderboard_entry)<transacts> := player_leaderboard_entry:
    Player := Src.Player
    Rank := Src.Rank
    Name := Src.Name
    PrimaryScore := Src.PrimaryScore
    SecondaryScore := Src.SecondaryScore

player_leaderboard_info := class:
    LeaderboardType:leaderboard_type = leaderboard_type.Player
    Title:string = ""
    PrimaryScoreType:player_leaderboard_stat_type = player_leaderboard_stat_type.Current
    SecondaryScoreType:player_leaderboard_stat_type = player_leaderboard_stat_type.Total
    Leaderboard:[]player_leaderboard_entry = array{}

player_stat_leaderboard_manager_device<public> := class(creative_device):

    @editable
    PlayerProfileManager:player_profile_manager = player_profile_manager{}

    @editable
    PersistentPlayerDataManager:persistent_player_data_manager = persistent_player_data_manager{}

    var CurrentLeaderboardIndex:int = 0

    GetLeaderboard(Agent:?agent, LeaderboardType:leaderboard_type, Title:string, StatID:player_stat_id, PrimaryScoreType:player_leaderboard_stat_type, SecondaryScoreType:player_leaderboard_stat_type, Count:int, SortOrder:sort_order):player_leaderboard_info =
        if (LeaderboardType = leaderboard_type.Player):
            LeaderboardFull:[]player_leaderboard_entry = GeneratePlayerLeaderboard(StatID, PrimaryScoreType, SecondaryScoreType)
            LeaderboardSorted:[]player_leaderboard_entry = SortLeaderboard(LeaderboardFull, SortOrder)
            LeaderboardFiltered:[]player_leaderboard_entry = FilterPlayerLeaderboard(LeaderboardSorted, Agent, Count)

            player_leaderboard_info{
                LeaderboardType:=LeaderboardType,
                Title:=Title,
                PrimaryScoreType:=PrimaryScoreType,
                SecondaryScoreType:=SecondaryScoreType,
                Leaderboard:=LeaderboardFiltered
            }
        else:
            LeaderboardFull:[]player_leaderboard_entry = GenerateTeamLeaderboard(StatID, PrimaryScoreType, SecondaryScoreType)
            LeaderboardSorted:[]player_leaderboard_entry = SortLeaderboard(LeaderboardFull, SortOrder)

            player_leaderboard_info{
                LeaderboardType:=LeaderboardType,
                Title:=Title,
                PrimaryScoreType:=PrimaryScoreType,
                SecondaryScoreType:=SecondaryScoreType,
                Leaderboard:=LeaderboardSorted
            }

    GeneratePlayerLeaderboard(StatID:player_stat_id, PrimaryScoreType:player_leaderboard_stat_type, SecondaryScoreType:player_leaderboard_stat_type):[]player_leaderboard_entry =
        var FullLeaderboard:[]player_leaderboard_entry = array{}

        var CurPlayerIndex:int = 1
        for (Player : Self.GetPlayspace().GetPlayers()):
            if:
                PersistentPlayerDataManager.CheckSaveDataForAgent[Player]
                SourceData := PersistentPlayerDataMap[Player]
                StatsInt := SourceData.StatsInt
                Stat := StatsInt[StatID]
            then:
                NewLeaderboardEntry:player_leaderboard_entry = player_leaderboard_entry{
                    Player:=option{Player}
                    Name:="PlayerName {CurPlayerIndex}" # TODO: how do we get the player user name?
                    PrimaryScore:=GetStatValueByType(Stat, PrimaryScoreType)
                    SecondaryScore:=GetStatValueByType(Stat, SecondaryScoreType)
                }
                set FullLeaderboard = FullLeaderboard + array{NewLeaderboardEntry}
                set CurPlayerIndex += 1

        FullLeaderboard

    GenerateTeamLeaderboard(StatID:player_stat_id, PrimaryScoreType:player_leaderboard_stat_type, SecondaryScoreType:player_leaderboard_stat_type):[]player_leaderboard_entry =
        var TeamStats:[team]player_stat_int = map{}

        for (Team : Self.GetPlayspace().GetTeamCollection().GetTeams()):
            if:
                TeamPlayers := Self.GetPlayspace().GetTeamCollection().GetAgents[Team]
                TeamPlayers.Length > 0
            then:
                if:
                    not TeamStats[Team]
                    set TeamStats[Team] = player_stat_int{}

                for (Agent : TeamPlayers):
                    if:
                        Player := player[Agent]
                    then:
                        PersistentPlayerDataManager.InitializePlayer(Player)

                        if:
                            PersistentPlayerDataManager.CheckSaveDataForAgent[Agent]
                            SourceData := PersistentPlayerDataMap[Player]
                            StatsInt := SourceData.StatsInt
                            Stat := StatsInt[StatID]
                            TeamStat := TeamStats[Team]
                            set TeamStats[Team] = player_stat_int:
                                MakeAggregatePlayerStatInt<constructor>(TeamStat, Stat)

        var FullLeaderboard:[]player_leaderboard_entry = array{}

        var CurTeamIndex:int = 1
        for (Team -> TeamStat : TeamStats):
            NewLeaderboardEntry:player_leaderboard_entry = player_leaderboard_entry{
                Name:="Team {CurTeamIndex}" # TODO: how do we get the team name?
                PrimaryScore:=GetStatValueByType(TeamStat, PrimaryScoreType)
                SecondaryScore:=GetStatValueByType(TeamStat, SecondaryScoreType)
            }
            set FullLeaderboard = FullLeaderboard + array{NewLeaderboardEntry}
            set CurTeamIndex += 1

        FullLeaderboard

    SortLeaderboard(Leaderboard:[]player_leaderboard_entry, SortOrder:sort_order):[]player_leaderboard_entry =
        if (SortOrder = sort_order.Ascending):
            UpdateLeaderboardRanks(Sort(Leaderboard, LessPlayerLeaderboardEntry))
        else:
            UpdateLeaderboardRanks(Sort(Leaderboard, GreaterPlayerLeaderboardEntry))

    Sort(Xs:[]t, Less(:t, :t)<decides><transacts>:void where t:type):[]t =
        ArrayLength := Xs.Length
        var Result:[]t = Xs
        for(i:=1..ArrayLength; negative_j:=-i..-1):
            j:=-negative_j
            if:
                Less[Result[j], Result[j-1]]
                Temp := Result[j]
                set Result[j] = Result[j-1] or Err()
                set Result[j-1] = Temp
        return Result

    LessPlayerLeaderboardEntry(X:player_leaderboard_entry, Y:player_leaderboard_entry)<decides><transacts>:void = X.PrimaryScore < Y.PrimaryScore
    GreaterPlayerLeaderboardEntry(X:player_leaderboard_entry, Y:player_leaderboard_entry)<decides><transacts>:void = X.PrimaryScore > Y.PrimaryScore

    UpdateLeaderboardRanks(Leaderboard:[]player_leaderboard_entry):[]player_leaderboard_entry =
        var RankedLeaderboard:[]player_leaderboard_entry = array{}
        var CurrentRank:int = 1

        for (Entry : Leaderboard):
            NewEntry := player_leaderboard_entry:
                MakePlayerLeaderboardEntry<constructor>(Entry)
                Rank:=CurrentRank
            set RankedLeaderboard = RankedLeaderboard + array{NewEntry}
            set CurrentRank += 1

        RankedLeaderboard

    FilterPlayerLeaderboard(Leaderboard:[]player_leaderboard_entry, MaybeAgent:?agent, Count:int):[]player_leaderboard_entry =
        if:
            Agent := MaybeAgent?
        then:
            FilteredLeaderboard:[]player_leaderboard_entry = for:
                Entry:Leaderboard
                not InvalidLeaderboardEntryForViewer[Entry, MaybeAgent, Count]
            do:
                Entry 
        else:
            FilteredLeaderboard:[]player_leaderboard_entry = for:
                Entry:Leaderboard
                not InvalidLeaderboardEntryRankOnly[Entry, Count]
            do:
                Entry     

    InvalidLeaderboardEntryRankOnly(Entry:player_leaderboard_entry, Count:int)<decides><transacts>:void=
            Entry.Rank > Count

    InvalidLeaderboardEntryForViewer(Entry:player_leaderboard_entry, MaybeAgent:?agent, Count:int)<decides><transacts>:void=
            Entry.Rank > Count
            Agent := MaybeAgent?
            Player := player[Agent]
            EntryPlayer := Entry.Player?
            EntryPlayer <> Player

    GetStatValueByType(Stat:player_stat_int, ScoreType:player_leaderboard_stat_type):int =
        var Score:int = 0
        case (ScoreType):
            player_leaderboard_stat_type.Current =>
                set Score = Stat.CurrentValue
            player_leaderboard_stat_type.StatMin =>
                set Score = Stat.MinValue
            player_leaderboard_stat_type.StatMax =>
                set Score = Stat.MaxValue
            player_leaderboard_stat_type.Total =>
                set Score = Stat.TotalValue
        Score
    
    GetPlayerLeaderboardStatTypeStr(StatType:player_leaderboard_stat_type):string=
        case (StatType):
            player_leaderboard_stat_type.Current =>
                "Cur"
            player_leaderboard_stat_type.StatMin =>
                "Min"
            player_leaderboard_stat_type.StatMax =>
                "Max"
            player_leaderboard_stat_type.Total =>
                "Tot"
    ActivateNextLeaderboard():void=    
        set CurrentLeaderboardIndex += 1
        if (CurrentLeaderboardIndex >= 6):
            set CurrentLeaderboardIndex = 0

    GetPlayerEliminationsLeaderboardInfo(Agent:?agent):player_leaderboard_info=
        GetLeaderboard(
            Agent,
            leaderboard_type.Player,
            "Eliminations",
            player_stat_id.EliminationsPlayers,
            player_leaderboard_stat_type.Current,
            player_leaderboard_stat_type.StatMax,
            3,
            sort_order.Descending
        )

    GetTeamEliminationsLeaderboardInfo(Agent:?agent):player_leaderboard_info=
        GetLeaderboard(
            Agent,
            leaderboard_type.Team,
            "Eliminations",
            player_stat_id.EliminationsPlayers,
            player_leaderboard_stat_type.Current,
            player_leaderboard_stat_type.StatMax,
            3,
            sort_order.Descending
        )

    GetCurrentLeaderboardInfo(Agent:?agent):player_leaderboard_info=
        var LeaderboardInfo:player_leaderboard_info = player_leaderboard_info{}
        case (CurrentLeaderboardIndex):
            0 =>
                set LeaderboardInfo = GetLeaderboard(
                    Agent,
                    leaderboard_type.Player,
                    "Gold Collected",
                    player_stat_id.GoldCollected,
                    player_leaderboard_stat_type.Current,
                    player_leaderboard_stat_type.Total,
                    3,
                    sort_order.Descending
                )
            1 =>
                set LeaderboardInfo = GetLeaderboard(
                    Agent,
                    leaderboard_type.Team,
                    "Players Killed",
                    player_stat_id.Deaths,
                    player_leaderboard_stat_type.Current,
                    player_leaderboard_stat_type.Total,
                    3,
                    sort_order.Ascending
                )
            2 =>
                set LeaderboardInfo = GetLeaderboard(
                    Agent,
                    leaderboard_type.Player,
                    "Minion Eliminations",
                    player_stat_id.EliminationsMinions,
                    player_leaderboard_stat_type.Current,
                    player_leaderboard_stat_type.StatMax,
                    3,
                    sort_order.Descending
                )
            3 =>
                set LeaderboardInfo = GetLeaderboard(
                    Agent,
                    leaderboard_type.Team,
                    "Minion Eliminations",
                    player_stat_id.EliminationsMinions,
                    player_leaderboard_stat_type.Current,
                    player_leaderboard_stat_type.StatMax,
                    3,
                    sort_order.Descending
                )
            4 =>
                set LeaderboardInfo = GetLeaderboard(
                    Agent,
                    leaderboard_type.Team,
                    "Towers Downed",
                    player_stat_id.TowersDowned,
                    player_leaderboard_stat_type.Current,
                    player_leaderboard_stat_type.StatMax,
                    3,
                    sort_order.Descending
                )
            5 =>
                set LeaderboardInfo = GetLeaderboard(
                    Agent,
                    leaderboard_type.Team,
                    "Shields Deactivated",
                    player_stat_id.ShieldsDeactivated,
                    player_leaderboard_stat_type.Current,
                    player_leaderboard_stat_type.StatMax,
                    3,
                    sort_order.Descending
                )
            _ => # default
                Print("No active leaderboard!")

        LeaderboardInfo