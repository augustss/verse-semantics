# Copyright Epic Games, Inc. All Rights Reserved.

using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }

verse_persistence_test_device<public> := class(creative_device):

    @editable
    PlayerProfileManager:player_profile_manager_device = player_profile_manager_device{}

    @editable
    StatusBillboard:billboard_device = billboard_device{}

    @editable
    AddXPSmallTrigger:trigger_device = trigger_device{}

    @editable
    AddXPMediumTrigger:trigger_device = trigger_device{}

    @editable
    AddXPLargeTrigger:trigger_device = trigger_device{}

    @editable
    CompleteQuestTrigger:trigger_device = trigger_device{}

    @editable
    ClearTrigger:trigger_device = trigger_device{}

    StatusMessage<localizes>(ActionCount:int, XP:int, Rank:string, CompletedQuestCount:int, QuestHistoryEntryCount:int):message = "Status[{ActionCount}]:\n  XP: {XP}\n  Rank: {Rank}\n  Completed Quests: {CompletedQuestCount}\n  History Entries: {QuestHistoryEntryCount}"

    OnBegin<override>():void =
        Print("VersePersistenceTestDevice OnBegin", ?Duration:=6.0)
        AddXPSmallTrigger.TriggeredEvent.Subscribe(AddXPSmallOptional)
        AddXPMediumTrigger.TriggeredEvent.Subscribe(AddXPMediumOptional)
        AddXPLargeTrigger.TriggeredEvent.Subscribe(AddXPLargeOptional)
        CompleteQuestTrigger.TriggeredEvent.Subscribe(CompleteQuestOptional)
        ClearTrigger.TriggeredEvent.Subscribe(ClearDataOptional)

        Self.GetPlayspace().PlayerAddedEvent().Subscribe(PlayerProfileManager.InitializePlayer)
        PlayerProfileManager.InitializePlayers()
        for (Player : Self.GetPlayspace().GetPlayers()):
            UpdateStatusBillboard(Player)
        Print("VersePersistenceTestDevice OnBegin complete", ?Duration:=6.0)

    ClearDataOptional(OptionalAgent:?agent):void= 
        if (Agent := OptionalAgent?):
            PlayerProfileManager.ClearPlayerProfile(Agent)
            UpdateStatusBillboard(Agent)
                    
    AddXPSmallOptional(OptionalAgent:?agent):void= 
        if (Agent := OptionalAgent?):
            PlayerProfileManager.GrantXPSmall(Agent)
            UpdateStatusBillboard(Agent)

    AddXPMediumOptional(OptionalAgent:?agent):void= 
        if (Agent := OptionalAgent?):
            PlayerProfileManager.GrantXPMedium(Agent)
            UpdateStatusBillboard(Agent)

    AddXPLargeOptional(OptionalAgent:?agent):void= 
        if (Agent := OptionalAgent?):
            PlayerProfileManager.GrantXPLarge(Agent)
            UpdateStatusBillboard(Agent)

    CompleteQuestOptional(OptionalAgent:?agent):void= 
        if (Agent := OptionalAgent?):
            PlayerProfileManager.CompleteQuest(Agent)
            UpdateStatusBillboard(Agent)

    UpdateStatusBillboard(Agent:agent):void=
        if:
            PlayerInfo := PlayerProfileManager.GetPlayerProfileInfo[Agent]
        then:
            StatusBillboard.SetText(StatusMessage(ActionCount:=PlayerInfo.ActionCount, XP:=PlayerInfo.XP, PlayerInfo.RankName, PlayerInfo.CompletedQuestCount, PlayerInfo.QuestHistoryEntryCount))

