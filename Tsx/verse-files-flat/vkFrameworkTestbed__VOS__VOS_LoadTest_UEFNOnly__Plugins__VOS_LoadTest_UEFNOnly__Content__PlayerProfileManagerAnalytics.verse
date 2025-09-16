# Copyright Epic Games, Inc. All Rights Reserved.

using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

player_profile_manager_analytics_log := class(log_channel){}

player_profile_manager_analytics<public> := class:

    AnalyticsLog:log = log{Channel := player_profile_manager_analytics_log}

    AnalyticsBillboard:billboard_device = billboard_device{}

    var QuestComplete:int = 0
    var QuestCompleteTotal:int = 0
    var GrantXp:int = 0
    var GrantXpTotal:int = 0
    var VerifyFits:int = 0
    var VerifyFitsTotal:int = 0

    var QuestCompleteRate:int = 0
    var GrantXpRate:int = 0

    AnalyticsMessage<localizes>(
        InQuestCompleteRate:int,
        InGrantXpRate:int,
        InQuestComplete:int,
        InQuestCompleteTotal:int,
        InGrantXp:int,
        InGrantXpTotal:int,
        InVerifyFits:int,
        InVerifyFitsTotal:int):message =
            "Analytics (1min)/(total):\n  QuestComplete[{InQuestCompleteRate}]: {InQuestComplete}/{InQuestCompleteTotal}\n  GrantXP[{InGrantXpRate}]: {InGrantXp}/{InGrantXpTotal}\n  VerifyFits: {InVerifyFits}/{InVerifyFitsTotal}"

    RecordQuestComplete():void=
        set QuestComplete += 1
        set QuestCompleteTotal += 1

    RecordGrantXp():void=
        set GrantXp += 1
        set GrantXpTotal += 1

    RecordVerifyFits():void=
        set VerifyFits += 1
        set VerifyFitsTotal += 1

    ResetWindowedData():void=
        set QuestComplete = 0
        set GrantXp = 0
        set VerifyFits = 0

    UpdateAnalyticsBillboard():void=
        AnalyticsBillboard.SetText(
            AnalyticsMessage(
                InQuestCompleteRate:=QuestCompleteRate,
                InGrantXpRate:=GrantXpRate,
                InQuestComplete:=QuestComplete,
                InQuestCompleteTotal:=QuestCompleteTotal,
                InGrantXp:=GrantXp,
                InGrantXpTotal:=GrantXpTotal,
                InVerifyFits:=VerifyFits,
                InVerifyFitsTotal:=VerifyFitsTotal
            )
        )

    RunUpdateLoop()<suspends>:void=
        sync:
            loop:
                UpdateAnalyticsBillboard()
                Sleep(1.0)
            loop:
                ResetWindowedData()
                UpdateAnalyticsBillboard()
                Sleep(60.0)