# Copyright Epic Games, Inc. All Rights Reserved.

using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }

leaderboard_writer_device<public> := class(creative_device):

    @editable
    PlayerStatLeaderboardManager:player_stat_leaderboard_manager_device = player_stat_leaderboard_manager_device{}

    EmptyMessage<localizes>():message = ""
    ElipsesMessage<localizes>():message = "...\n"

    LeaderboardPlayerEntryMessage<localizes>(
        Rank:string,
        Agent:agent,
        PrimaryScore:string,
        SecondaryScore:string
        ):message = "{PrimaryScore}   {Agent}\n"
        #):message = "{Rank}. {Agent}: {PrimaryScore} ({SecondaryScore})\n"

    LeaderboardTeamEntryMessage<localizes>(
        Rank:string,
        Name:string,
        PrimaryScore:string,
        SecondaryScore:string
        ):message = "{PrimaryScore}   {Name}\n"
        #):message = "{Rank}. {Name}: {PrimaryScore} ({SecondaryScore})\n"

    LeaderboardDisplayMessage<localizes>(
        Title:string,
        PrimaryTypeString:string,
        SecondaryTypeString:string,
        Row1:message,
        Row2:message,
        Row3:message,
        Row4:message,
        Row5:message
        ):message = "{Title}\n {Row1} {Row2} {Row3} {Row4} {Row5}"
        #):message = "{Title} {PrimaryTypeString}({SecondaryTypeString})\n {Row1} {Row2} {Row3} {Row4} {Row5}"

    GetFixedWidthIntString(Value:int, Width:int):string=
        var LeadSpace:string = ""
        if (Width = 3):
            set LeadSpace = " "
        else if (Width = 4):
            set LeadSpace = "  "
        if (Width = 5):
            set LeadSpace = "   "
        if (Width = 6):
            set LeadSpace = "    "

        if (Value < 10):
            "{LeadSpace} {Value}"
        else if (Value < 100):
            "{LeadSpace}{Value}"
        else:
            "{Value}"

    GetLeaderboardMessage(LeaderboardInfo:player_leaderboard_info):message= 
        var LeaderboardMessage:message = EmptyMessage()

        if (LeaderboardInfo.LeaderboardType = leaderboard_type.Player):
            Row1Msg:message = GetPlayerRowMessage(LeaderboardInfo, 0)
            Row2Msg:message = GetPlayerRowMessage(LeaderboardInfo, 1)
            Row3Msg:message = GetPlayerRowMessage(LeaderboardInfo, 2)
            var Row4Msg:message = GetPlayerRowMessage(LeaderboardInfo, 3)
            var Row5Msg:message = GetPlayerRowMessage(LeaderboardInfo, 4)
            var Row6Msg:message = GetPlayerRowMessage(LeaderboardInfo, 5)

            if:
                Entry := LeaderboardInfo.Leaderboard[5]
                Entry.Rank > 5
            then:
                set Row5Msg = Row6Msg
                set Row4Msg = ElipsesMessage()

            set LeaderboardMessage = LeaderboardDisplayMessage(
                Title:=LeaderboardInfo.Title,
                PrimaryTypeString:=PlayerStatLeaderboardManager.GetPlayerLeaderboardStatTypeStr(LeaderboardInfo.PrimaryScoreType),
                SecondaryTypeString:=PlayerStatLeaderboardManager.GetPlayerLeaderboardStatTypeStr(LeaderboardInfo.SecondaryScoreType),
                Row1:=Row1Msg,
                Row2:=Row2Msg,
                Row3:=Row3Msg,
                Row4:=Row4Msg,
                Row5:=Row5Msg
            )
        else:
            Row1Msg:message = GetTeamRowMessage(LeaderboardInfo, 0)
            Row2Msg:message = GetTeamRowMessage(LeaderboardInfo, 1)
            Row3Msg:message = GetTeamRowMessage(LeaderboardInfo, 2)
            Row4Msg:message = GetTeamRowMessage(LeaderboardInfo, 3)

            set LeaderboardMessage = LeaderboardDisplayMessage(
                Title:=LeaderboardInfo.Title,
                PrimaryTypeString:=PlayerStatLeaderboardManager.GetPlayerLeaderboardStatTypeStr(LeaderboardInfo.PrimaryScoreType),
                SecondaryTypeString:=PlayerStatLeaderboardManager.GetPlayerLeaderboardStatTypeStr(LeaderboardInfo.SecondaryScoreType),
                Row1:=Row1Msg,
                Row2:=Row2Msg,
                Row3:=Row3Msg,
                Row4:=Row4Msg,
                Row5:=EmptyMessage()
            )

        LeaderboardMessage
    
    GetPlayerRowMessage(LeaderboardInfo:player_leaderboard_info, Index:int):message= 
        var RowMsg:message = EmptyMessage()
        if:
            Entry := LeaderboardInfo.Leaderboard[Index]
            EntryPlayer := Entry.Player?
            EntryAgent := agent[EntryPlayer]
        then:
            RankStr:string = GetFixedWidthIntString(Entry.Rank, 2)
            PrimaryScoreStr:string = GetFixedWidthIntString(Entry.PrimaryScore, 4)
            SecondaryScoreStr:string = GetFixedWidthIntString(Entry.SecondaryScore, 4)
            set RowMsg = LeaderboardPlayerEntryMessage(Rank:=RankStr, Agent:=EntryAgent, PrimaryScore:=PrimaryScoreStr, SecondaryScore:=SecondaryScoreStr)
        RowMsg
    
    GetTeamRowMessage(LeaderboardInfo:player_leaderboard_info, Index:int):message= 
        var RowMsg:message = EmptyMessage()
        if:
            Entry := LeaderboardInfo.Leaderboard[Index]
        then:
            RankStr:string = GetFixedWidthIntString(Entry.Rank, 2)
            PrimaryScoreStr:string = GetFixedWidthIntString(Entry.PrimaryScore, 4)
            SecondaryScoreStr:string = GetFixedWidthIntString(Entry.SecondaryScore, 4)
            set RowMsg = LeaderboardTeamEntryMessage(Rank:=RankStr, Name:=Entry.Name, PrimaryScore:=PrimaryScoreStr, SecondaryScore:=SecondaryScoreStr)
        RowMsg