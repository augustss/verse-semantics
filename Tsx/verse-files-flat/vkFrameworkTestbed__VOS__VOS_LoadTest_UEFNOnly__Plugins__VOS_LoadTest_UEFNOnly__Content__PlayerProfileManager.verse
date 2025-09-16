# Copyright Epic Games, Inc. All Rights Reserved.

using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }

player_class := enum<persistable>{
    Villager,
    Fighter,
    Ranger,
    Cleric,
    Wizard
}

player_events_struct := struct<computes><persistable>:
    Events:[]string = array{}

player_profile_data := class<final><persistable>:
    Class:player_class = player_class.Villager
    XP:int = 0
    #Mana:int = 0
    Mana:float = 0.0
    Rank:int = 0
    CompletedQuestCount:int = 0
    QuestHistory:[]string = array{}
    #Position:vector3 = vector3{}
    EventLog:player_events_struct = player_events_struct{}


MakePlayerProfileData<constructor>(Src:player_profile_data)<transacts> := player_profile_data:
    Class := Src.Class
    XP := Src.XP
    Mana := Src.Mana
    Rank := Src.Rank
    CompletedQuestCount := Src.CompletedQuestCount
    QuestHistory := Src.QuestHistory
    #Position := Src.Position
    EventLog := Src.EventLog
    
player_profile_info := struct:
    ActionCount:int = 0
    XP:int = 0
    Mana:float = 0.0
    Rank:int = 0
    CompletedQuestCount:int = 0
    QuestHistoryEntryCount:int = 0
    RankName:string = ""
    #Position:vector3 = vector3{}
    EventLogCount:int = 0


rank_data := struct:
    RankName:string = ""
    RequiredXP:int = 0

var PlayerActionCountMap:weak_map(player, int) = map{}
var PlayerProfileDataMap:weak_map(player, player_profile_data) = map{}

player_profile_log := class(log_channel){}

player_profile_manager_device<public> := class(creative_device):

    PlayerProfileLog:log = log{Channel := player_profile_log}

    QuestDataManager:quest_data_manager = quest_data_manager{}

    ItemDataManager:item_data_manager = item_data_manager{}

    var CurrentQuestIndex:int = 0

    var Analytics<public>:player_profile_manager_analytics = player_profile_manager_analytics{}

    Ranks:[]rank_data = 
        array:
            rank_data{RankName:="n00b", RequiredXP:=0}
            rank_data{RankName:="Rookie", RequiredXP:=100}
            rank_data{RankName:="Pirate", RequiredXP:=1000}
            rank_data{RankName:="Master", RequiredXP:=10000}
            rank_data{RankName:="Ninja", RequiredXP:=100000}
            rank_data{RankName:="Jedi", RequiredXP:=1000000}

    SetAnalytics(InAnalytics:player_profile_manager_analytics):void=
        set Analytics = InAnalytics
        spawn{ Analytics.RunUpdateLoop() }

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
            ClassString := GetClassString(ProfileData.Class)
            FullRankName := Ranks[ProfileData.Rank].RankName + " " + ClassString
            set ProfileInfo = player_profile_info{
                ActionCount:=ActionCount,
                XP:=ProfileData.XP,
                Mana:=ProfileData.Mana,
                Rank:=ProfileData.Rank,
                CompletedQuestCount:=ProfileData.CompletedQuestCount,
                QuestHistoryEntryCount:=ProfileData.QuestHistory.Length,
                RankName:=FullRankName,
                #Position:=ProfileData.Position
                EventLogCount:=ProfileData.EventLog.Events.Length
            }
        ProfileInfo
    
    CompleteQuest(Agent:agent):void= 
        if:
            CheckSaveDataForPlayer[Agent]
            Player := player[Agent]
            SaveData := PlayerProfileDataMap[Player]
            CurrentQuest := QuestDataManager.Quests[CurrentQuestIndex]
        then:
            GrantXP(Agent, CurrentQuest.RewardXP)
            RecordQuestHistory(Agent, CurrentQuest.QuestHistory)
            RecordAction(Agent)
            Analytics.RecordQuestComplete()
            set CurrentQuestIndex += 1
            if (CurrentQuestIndex = QuestDataManager.Quests.Length):
                set CurrentQuestIndex = 0

    SetQuestHistory(Src:player_profile_data, NewQuestHistory:[]string)<transacts>:player_profile_data =
        NewData:player_profile_data = player_profile_data:
            MakePlayerProfileData<constructor>(Src)
            QuestHistory := NewQuestHistory

    RecordQuestHistory(Agent:agent, QuestHistory:string):void= 
        if:
            CheckSaveDataForPlayer[Agent]
            Player := player[Agent]
            SaveData := PlayerProfileDataMap[Player]
        then:
            # try the golden path. Confirm updated struct still FitsInPlayerMap, then update the live struct.
            Analytics.RecordVerifyFits()
            if:
                var UpdatedSaveData:player_profile_data = SaveData
                set UpdatedSaveData = SetQuestHistory(UpdatedSaveData, array{QuestHistory} + SaveData.QuestHistory)
                FitsInPlayerMap[UpdatedSaveData]
                set PlayerProfileDataMap[Player] = player_profile_data:
                    MakePlayerProfileData<constructor>(SaveData)
                    QuestHistory := UpdatedSaveData.QuestHistory
                    CompletedQuestCount := SaveData.CompletedQuestCount + 1
                    EventLog := player_events_struct{
                        Events:=UpdatedSaveData.EventLog.Events + array{"Q"}
                    }
            # the new data is too big to fit in the player map, update the data until it now fits.
            else:
                var TestUpdatedSaveData:player_profile_data = SaveData
                set TestUpdatedSaveData = SetQuestHistory(TestUpdatedSaveData, array{})
                var UpdatedSaveData:player_profile_data = TestUpdatedSaveData

                ProposedQuestHistory:[]string = array{QuestHistory} + SaveData.QuestHistory
                var DataFits:logic = true
                for (CurQuestHistory : ProposedQuestHistory, DataFits?):
                    #set TestUpdatedSaveData.QuestHistory = for(X:=SaveData.QuestHistory[0..SaveData.QuestHistory.Length-2]). X
                    #set QuestHistory:[]string = for:
                    #    Index->CurHistory:QuestHistory
                    #    Index<QuestHistory.Length-2

                    set TestUpdatedSaveData = SetQuestHistory(TestUpdatedSaveData, array{CurQuestHistory})
                    Analytics.RecordVerifyFits()
                    if (FitsInPlayerMap[TestUpdatedSaveData]):
                        set UpdatedSaveData = SetQuestHistory(UpdatedSaveData, TestUpdatedSaveData.QuestHistory)
                    #else:
                    if:
                        SourceData := PlayerProfileDataMap[Player]
                        set PlayerProfileDataMap[Player] = player_profile_data:
                            MakePlayerProfileData<constructor>(SourceData)
                            QuestHistory := UpdatedSaveData.QuestHistory
                            CompletedQuestCount := SourceData.CompletedQuestCount + 1
                            EventLog := player_events_struct{
                                Events:=UpdatedSaveData.EventLog.Events + array{"Q"}
                            }
                    set DataFits = false

    RecordAction(Agent:agent):void= 
        if:
            Player := player[Agent]
            set PlayerActionCountMap[Player] += 1

    GrantXP(Agent:agent, GrantedXP:int):void= 
        if:
            CheckSaveDataForPlayer[Agent]
            Player := player[Agent]
            SourceData := PlayerProfileDataMap[Player]
            set PlayerProfileDataMap[Player] = player_profile_data:
                MakePlayerProfileData<constructor>(SourceData)
                XP := SourceData.XP + GrantedXP
                Mana := SourceData.Mana + 2.0
                #Position := vector3{ X:=GrantedXP, Y:=GrantedXP, Z:=GrantedXP}
                EventLog := player_events_struct{
                    Events:=SourceData.EventLog.Events + array{"Q"}
                }
            EarnedRanks := UpdateRank[Player]
            SaveData := PlayerProfileDataMap[Player]
        then:
            Analytics.RecordGrantXp()
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
                    SourceData := PlayerProfileDataMap[Player]
                    set PlayerProfileDataMap[Player] = player_profile_data:
                        MakePlayerProfileData<constructor>(SourceData)
                        Rank := RankIndex
        EarnedRanks

    GetClassString(Class:player_class)<computes>:string=
        case(Class):
            player_class.Villager =>
                "Villager"
            player_class.Fighter =>
                "Fighter"
            player_class.Ranger =>
                "Ranger"
            player_class.Cleric =>
                "Cleric"
            player_class.Wizard =>
                "Wizard"

    UpdateClass(Agent:agent):void= 
        if:
            CheckSaveDataForPlayer[Agent]
            Player := player[Agent]
            PlayerProfile := PlayerProfileDataMap[Player]
            NewClass:player_class = GetNextClass(PlayerProfile.Class)
            set PlayerProfileDataMap[Player] = player_profile_data:
                MakePlayerProfileData<constructor>(PlayerProfile)
                Class := NewClass

    GetNextClass(CurClass:player_class)<transacts>:player_class =
        if (CurClass = player_class.Villager):
            player_class.Fighter
        else if (CurClass = player_class.Fighter):
            player_class.Ranger
        else if (CurClass = player_class.Ranger):
            player_class.Cleric
        else if (CurClass = player_class.Cleric):
            player_class.Wizard
        else if (CurClass = player_class.Wizard):
            player_class.Fighter
        else:
            player_class.Villager
    