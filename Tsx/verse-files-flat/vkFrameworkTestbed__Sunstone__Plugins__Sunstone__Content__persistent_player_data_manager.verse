# Copyright Epic Games, Inc. All Rights Reserved.

using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }

# WARNING: Do not remove any enum entries from this enum. It is persistable
# and we need the slot names to match up to old persistence data or the backend
# will crash. If you're removing a persistable piece of data just leave the slot
# in place and add a comment that it shouldn't be used anymore.
player_stat_id := enum<persistable>{
    TimePlayedSeconds,   # DEPRECATED
    SurvivalTimeSeconds, # DEPRECATED 
    WinCount,
    LoseCount,
    GoldCollected,
    GoldSpent,           # DEPRECATED
    EliminationsPlayers,
    EliminationsMinions,
    Deaths,
    TowersDowned,
    TowersUpgraded,
    ShieldsDeactivated,
    ClassInfantryCount,
    ClassHeavyCount,
    ClassScoutCount
}

player_stat_int := class<final><persistable>:
    CurrentValue:int = 0
    MinValue:int = 100000000
    MaxValue:int = 0
    TotalValue:int = 0

    SetMinValue(NewValue:int)<transacts>:int=
        if (NewValue < MinValue):
            NewValue
        else:
            MinValue

    SetMaxValue(NewValue:int)<transacts>:int=
        if (NewValue > MaxValue):
            NewValue
        else:
            MaxValue

    SetTotalValue(NewValue:int)<transacts>:int=
        NewValue + TotalValue

MakeUpdatedPlayerStatInt<constructor>(Src:player_stat_int, NewValue:int)<transacts> := player_stat_int:
    CurrentValue := Src.CurrentValue + NewValue
    MinValue := Src.MinValue # min stats should only be updated during MakeEndAggregation events.
    MaxValue := Src.SetMaxValue(Src.CurrentValue + NewValue)
    TotalValue := Src.TotalValue + NewValue

MakeAggregatePlayerStatInt<constructor>(Src1:player_stat_int, Src2:player_stat_int)<transacts> := player_stat_int:
    CurrentValue := Src1.CurrentValue + Src2.CurrentValue
    MinValue := Src1.SetMinValue(Src2.MinValue)
    MaxValue := Src1.SetMaxValue(Src2.MaxValue)
    TotalValue := Src1.TotalValue + Src2.TotalValue

MakeStartAggregationPlayerStatInt<constructor>(Src:player_stat_int)<transacts> := player_stat_int:
    CurrentValue := 0
    MinValue := Src.MinValue
    MaxValue := Src.MaxValue
    TotalValue := Src.TotalValue

MakeEndAggregationPlayerStatInt<constructor>(Src:player_stat_int)<transacts> := player_stat_int:
    CurrentValue := 0
    MinValue := Src.SetMinValue(Src.CurrentValue)
    MaxValue := Src.SetMaxValue(Src.CurrentValue)
    TotalValue := Src.TotalValue + Src.CurrentValue

persistent_player_data := class<final><persistable>:
    Version:int = 0
    StatsInt:[player_stat_id]player_stat_int = map{}

MakeDefaultPersistentPlayerData<constructor>()<transacts> := persistent_player_data:
    Version := 1
    StatsInt := map:
        player_stat_id.WinCount => player_stat_int{}
        player_stat_id.LoseCount => player_stat_int{}
        player_stat_id.GoldCollected => player_stat_int{}
        player_stat_id.EliminationsPlayers => player_stat_int{}
        player_stat_id.EliminationsMinions => player_stat_int{}
        player_stat_id.Deaths => player_stat_int{}
        player_stat_id.TowersDowned => player_stat_int{}
        player_stat_id.TowersUpgraded => player_stat_int{}
        player_stat_id.ShieldsDeactivated => player_stat_int{}
        player_stat_id.ClassInfantryCount => player_stat_int{}
        player_stat_id.ClassHeavyCount => player_stat_int{}
        player_stat_id.ClassScoutCount => player_stat_int{}

MakePersistentPlayerData<constructor>(Src:persistent_player_data)<transacts> := persistent_player_data:
    Version := Src.Version
    StatsInt := Src.StatsInt

player_stats_info := class:
    WinCount:int = 0
    LoseCount:int = 0
    ClassInfantryMatch:int = 0
    ClassInfantryTotal:int = 0
    ClassHeavyMatch:int = 0
    ClassHeavyTotal:int = 0
    ClassScoutMatch:int = 0
    ClassScoutTotal:int = 0
    GoldCollectedMatch:int = 0
    GoldCollectedTotal:int = 0
    GoldSpentMatch:int = 0
    GoldSpentTotal:int = 0
    DeathsMatch:int = 0
    DeathsTotal:int = 0
    TowersUpgradedMatch:int = 0
    TowersUpgradedTotal:int = 0
    TowersDownedMatch:int = 0
    TowersDownedTotal:int = 0
    ShieldsDeactivatedMatch:int = 0
    ShieldsDeactivatedTotal:int = 0
    EliminationsPlayersMatch:int = 0
    EliminationsPlayersTotal:int = 0
    EliminationsMinionsMatch:int = 0
    EliminationsMinionsTotal:int = 0

var PersistentPlayerDataMap:weak_map(player, persistent_player_data) = map{}

persistent_player_data_log := class(log_channel){}

persistent_player_data_manager<public> := class(creative_device):

    PersistentPlayerDataLog:log = log{Channel := persistent_player_data_log}

    InitializePlayer(Player:player):void=
        if:
            not PersistentPlayerDataMap[Player]
            set PersistentPlayerDataMap[Player] = MakeDefaultPersistentPlayerData()

    CheckSaveDataForAgent(Agent:agent)<decides><transacts>:void=
        if(Player := player[Agent]):
            if(PersistentPlayerDataMap[Player]):
                # no-op
            else:
                set PersistentPlayerDataMap[Player] = MakeDefaultPersistentPlayerData()

    ClearPlayerProfile(Agent:agent):void= 
        if:
            CheckSaveDataForAgent[Agent]
            Player := player[Agent]
            set PersistentPlayerDataMap[Player] = MakeDefaultPersistentPlayerData()
        then:
            PersistentPlayerDataLog.Print("Agent data cleared!")
                    
    GetPlayerStatInfo(Agent:agent):player_stats_info=
        var StatsInfo:player_stats_info = player_stats_info{}
        if:
            CheckSaveDataForAgent[Agent]
            Player := player[Agent]
            SourceData := PersistentPlayerDataMap[Player]
            WinCountStat := SourceData.StatsInt[player_stat_id.WinCount]
            LoseCountStat := SourceData.StatsInt[player_stat_id.LoseCount]
            ClassInfantryStat := SourceData.StatsInt[player_stat_id.ClassInfantryCount]
            ClassHeavyStat := SourceData.StatsInt[player_stat_id.ClassHeavyCount]
            ClassScoutStat := SourceData.StatsInt[player_stat_id.ClassScoutCount]
            GoldCollectedStat := SourceData.StatsInt[player_stat_id.GoldCollected]
            DeathsStat := SourceData.StatsInt[player_stat_id.Deaths]
            TowersUpgradedStat := SourceData.StatsInt[player_stat_id.TowersUpgraded]
            TowersDownedStat := SourceData.StatsInt[player_stat_id.TowersDowned]
            ShieldsDeactivatedStat := SourceData.StatsInt[player_stat_id.ShieldsDeactivated]
            EliminationsPlayersStat := SourceData.StatsInt[player_stat_id.EliminationsPlayers]
            EliminationsMinionsStat := SourceData.StatsInt[player_stat_id.EliminationsMinions]
            set StatsInfo = player_stats_info{
                WinCount:=WinCountStat.TotalValue,
                LoseCount:=LoseCountStat.TotalValue,
                ClassInfantryMatch:=ClassInfantryStat.CurrentValue,
                ClassInfantryTotal:=ClassInfantryStat.TotalValue,
                ClassHeavyMatch:=ClassHeavyStat.CurrentValue,
                ClassHeavyTotal:=ClassHeavyStat.TotalValue,
                ClassScoutMatch:=ClassScoutStat.CurrentValue,
                ClassScoutTotal:=ClassScoutStat.TotalValue,
                GoldCollectedMatch:=GoldCollectedStat.CurrentValue,
                GoldCollectedTotal:=GoldCollectedStat.TotalValue,
                DeathsMatch:=DeathsStat.CurrentValue,
                DeathsTotal:=DeathsStat.TotalValue,
                TowersUpgradedMatch:=TowersUpgradedStat.CurrentValue,
                TowersUpgradedTotal:=TowersUpgradedStat.TotalValue,
                TowersDownedMatch:=TowersDownedStat.CurrentValue,
                TowersDownedTotal:=TowersDownedStat.TotalValue,
                ShieldsDeactivatedMatch:=ShieldsDeactivatedStat.CurrentValue,
                ShieldsDeactivatedTotal:=ShieldsDeactivatedStat.TotalValue,
                EliminationsPlayersMatch:=EliminationsPlayersStat.CurrentValue,
                EliminationsPlayersTotal:=EliminationsPlayersStat.TotalValue,
                EliminationsMinionsMatch:=EliminationsMinionsStat.CurrentValue,
                EliminationsMinionsTotal:=EliminationsMinionsStat.TotalValue
            }

        StatsInfo 

    StartMatch(Agent:agent):void= 
        if:
            CheckSaveDataForAgent[Agent]
            Player := player[Agent]
            SourceData := PersistentPlayerDataMap[Player]
            StatsInt := SourceData.StatsInt
        then:
            var UpdatedStatsInt:[player_stat_id]player_stat_int = SourceData.StatsInt
            for (Key->Value:StatsInt):
                if:
                    set UpdatedStatsInt[Key] = MakeStartAggregationPlayerStatInt(Value)
            if:
                set PersistentPlayerDataMap[Player] = persistent_player_data:
                    MakePersistentPlayerData<constructor>(SourceData)
                    StatsInt := UpdatedStatsInt

    EndMatch(Agent:agent):void= 
        if:
            CheckSaveDataForAgent[Agent]
            Player := player[Agent]
            SourceData := PersistentPlayerDataMap[Player]
            StatsInt := SourceData.StatsInt
        then:
            var UpdatedStatsInt:[player_stat_id]player_stat_int = SourceData.StatsInt
            for (Key->Value:StatsInt):
                if:
                    set UpdatedStatsInt[Key] = MakeEndAggregationPlayerStatInt(Value)
            if:
                set PersistentPlayerDataMap[Player] = persistent_player_data:
                    MakePersistentPlayerData<constructor>(SourceData)
                    StatsInt := UpdatedStatsInt

    RecordStat(Agent:agent, StatID:player_stat_id, NewValue:int):void= 
        if:
            CheckSaveDataForAgent[Agent]
            Player := player[Agent]
            SourceData := PersistentPlayerDataMap[Player]
            StatsInt := SourceData.StatsInt
        then:
            var UpdatedStatsInt:[player_stat_id]player_stat_int = SourceData.StatsInt
            if:
                CurrentStat := UpdatedStatsInt[StatID]
                set UpdatedStatsInt[StatID] = MakeUpdatedPlayerStatInt(CurrentStat, NewValue)
                set PersistentPlayerDataMap[Player] = persistent_player_data:
                    MakePersistentPlayerData<constructor>(SourceData)
                    StatsInt := UpdatedStatsInt
