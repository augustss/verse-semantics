# Copyright Epic Games, Inc. All Rights Reserved.

using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }

player_profile_data := struct<concrete><computes><persistable>:
    XP:int = 0
    Rank:int = 0
    CompletedQuestCount:int = 0
    QuestHistory:[]string = array{}
    
player_profile_info := struct:
    ActionCount:int = 0
    XP:int = 0
    Rank:int = 0
    CompletedQuestCount:int = 0
    QuestHistoryEntryCount:int = 0
    RankName:string = ""

rank_data := struct:
    RankName:string = ""
    RequiredXP:int = 0

var PlayerActionCountMap:weak_map(player, int) = map{}
var PlayerProfileDataMap:weak_map(player, player_profile_data) = map{}

player_profile_log := class(log_channel){}

player_profile_manager_device<public> := class(creative_device):

    PlayerProfileLog:log = log{Channel := player_profile_log}

    QuestDataManager:quest_data_manager = quest_data_manager{}

    Ranks:[]rank_data = 
        array:
            rank_data{RankName:="n00b", RequiredXP:=0}
            rank_data{RankName:="Rookie", RequiredXP:=100}
            rank_data{RankName:="Pirate", RequiredXP:=1000}
            rank_data{RankName:="Master", RequiredXP:=10000}
            rank_data{RankName:="Ninja", RequiredXP:=100000}
            rank_data{RankName:="Jedi", RequiredXP:=1000000}

    InitializePlayers():void =
        for (Player : Self.GetPlayspace().GetPlayers()):
            InitializePlayer(Player)

    InitializePlayer(Player:player):void=
        if:
            not PlayerProfileDataMap[Player]
            set PlayerProfileDataMap[Player] = player_profile_data{}
        #then:
        #    PlayerProfileLog.Print("Setting player save data to default!")
        if:
            not PlayerActionCountMap[Player]
            set PlayerActionCountMap[Player] = 0
        #then:
        #    PlayerProfileLog.Print("Setting player save data to default!")

    CheckSaveDataForPlayer(Agent:agent)<decides><transacts>:void=
        if(Player := player[Agent]):
            if(PlayerProfileDataMap[Player]):
                # no-op
            else:
                set PlayerProfileDataMap[Player] = player_profile_data{}

    ClearPlayerProfile(Agent:agent):void= 
        if:
            CheckSaveDataForPlayer[Agent]
            Player := player[Agent]
            set PlayerProfileDataMap[Player] = player_profile_data{}
        then:
            PlayerProfileLog.Print("Agent data cleared!")
                    
    GetPlayerProfileInfo(Agent:agent)<decides><transacts>:player_profile_info= 
        var ProfileInfo:player_profile_info = player_profile_info{}
        if:
            CheckSaveDataForPlayer[Agent]
            Player := player[Agent]
            ProfileData := PlayerProfileDataMap[Player]
            ActionCount := PlayerActionCountMap[Player]
            set ProfileInfo = player_profile_info{
                ActionCount:=ActionCount,
                XP:=ProfileData.XP,
                Rank:=ProfileData.Rank,
                CompletedQuestCount:=ProfileData.CompletedQuestCount,
                QuestHistoryEntryCount:=ProfileData.QuestHistory.Length,
                RankName:=Ranks[ProfileData.Rank].RankName
            }
        ProfileInfo
    
    CompleteQuest(Agent:agent):void= 
        if:
            CheckSaveDataForPlayer[Agent]
            Player := player[Agent]
            SaveData := PlayerProfileDataMap[Player]
            CurrentQuest := QuestDataManager.Quests[SaveData.CompletedQuestCount]
        then:
            GrantXP(Agent, CurrentQuest.RewardXP)
            RecordQuestHistory(Agent, CurrentQuest.QuestHistory)
            RecordAction(Agent)

    RecordQuestHistory(Agent:agent, QuestHistory:string):void= 
        if:
            CheckSaveDataForPlayer[Agent]
            Player := player[Agent]
            SaveData := PlayerProfileDataMap[Player]
        then:
            # try the golden path. Confirm updated struct still FitsInPlayerMap, then update the live struct.
            if:
                var UpdatedSaveData:player_profile_data = SaveData
                set UpdatedSaveData.QuestHistory = array{QuestHistory} + SaveData.QuestHistory
                FitsInPlayerMap[UpdatedSaveData]
                set PlayerProfileDataMap[Player].QuestHistory = UpdatedSaveData.QuestHistory
                set PlayerProfileDataMap[Player].CompletedQuestCount += 1
            # the new data is too big to fit in the player map, update the data until it now fits.
            else:
                var TestUpdatedSaveData:player_profile_data = SaveData
                set TestUpdatedSaveData.QuestHistory = array{}
                var UpdatedSaveData:player_profile_data = TestUpdatedSaveData

                ProposedQuestHistory:[]string = array{QuestHistory} + SaveData.QuestHistory
                var DataFits:logic = true
                for (CurQuestHistory : ProposedQuestHistory, DataFits?):
                    #set TestUpdatedSaveData.QuestHistory = for(X:=SaveData.QuestHistory[0..SaveData.QuestHistory.Length-2]). X
                    #set QuestHistory:[]string = for:
                    #    Index->CurHistory:QuestHistory
                    #    Index<QuestHistory.Length-2

                    set TestUpdatedSaveData.QuestHistory += array{CurQuestHistory}
                    if (FitsInPlayerMap[TestUpdatedSaveData]):
                        set UpdatedSaveData.QuestHistory = TestUpdatedSaveData.QuestHistory
                    else:
                        if:
                            set PlayerProfileDataMap[Player].QuestHistory = UpdatedSaveData.QuestHistory
                            set PlayerProfileDataMap[Player].CompletedQuestCount += 1
                        set DataFits = false

    RecordAction(Agent:agent):void= 
        if:
            Player := player[Agent]
            set PlayerActionCountMap[Player] += 1

    GrantXP(Agent:agent, GrantedXP:int):void= 
        if:
            CheckSaveDataForPlayer[Agent]
            Player := player[Agent]
            set PlayerProfileDataMap[Player].XP += GrantedXP
            EarnedRanks := UpdateRank[Player]
            SaveData := PlayerProfileDataMap[Player]
        #then:
        #    if (EarnedRanks > 0):
        #        PlayerProfileLog.Print("You earned a new rank! EarnedRanks[{EarnedRanks}]")

    GrantXPSmall(Agent:agent):void= 
        GrantXP(Agent, 10)

    GrantXPMedium(Agent:agent):void= 
        GrantXP(Agent, 100)

    GrantXPLarge(Agent:agent):void= 
        GrantXP(Agent, 1000)

    GrantXPXLarge(Agent:agent):void= 
        GrantXP(Agent, 10000)

    UpdateRank(Player:player)<decides><transacts>:int= 
        var EarnedRanks:int = 0
        if:
            PlayerProfile := PlayerProfileDataMap[Player]
        then:
            OriginalRank := PlayerProfile.Rank
            for(RankIndex := OriginalRank+1..Ranks.Length):
                if (PlayerProfile.XP >= Ranks[RankIndex].RequiredXP):
                    set EarnedRanks += 1
                    set PlayerProfileDataMap[Player].Rank = RankIndex
        EarnedRanks
