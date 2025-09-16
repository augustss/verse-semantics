# Copyright Epic Games, Inc. All Rights Reserved.

using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }

verse_persistence_game_device<public> := class(creative_device):

    @editable
    PlayerProfileManager:player_profile_manager_device = player_profile_manager_device{}

    @editable
    StatusBillboard:billboard_device = billboard_device{}


    @editable
    UpdateClassButton:button_device = button_device{}

    @editable
    AddXPSmallButton:button_device = button_device{}

    @editable
    AddXPMediumButton:button_device = button_device{}

    @editable
    AddXPLargeButton:button_device = button_device{}

    @editable
    CompleteQuestButton:button_device = button_device{}

    @editable
    ClearButton:button_device = button_device{}


    StatusMessage<localizes>(
        ActionCount:int,
        XP:int,
        #Mana:int,
        Mana:float,
        Rank:string,
        CompletedQuestCount:int,
        QuestHistoryEntryCount:int,
        EventLogCount:int
        ):message = "Status[{ActionCount}]:\n  XP: {XP} Mana: {Mana}\n  Rank: {Rank}\n  Completed Quests: {CompletedQuestCount}\n  Counts: QH:{QuestHistoryEntryCount}, EL:{EventLogCount}"

    OnBegin<override>():void =
        UpdateClassButton.InteractedWithEvent.Subscribe(UpdateClass)
        AddXPSmallButton.InteractedWithEvent.Subscribe(AddXPSmall)
        AddXPMediumButton.InteractedWithEvent.Subscribe(AddXPMedium)
        AddXPLargeButton.InteractedWithEvent.Subscribe(AddXPLarge)
        CompleteQuestButton.InteractedWithEvent.Subscribe(CompleteQuest)
        ClearButton.InteractedWithEvent.Subscribe(ClearData)

        Self.GetPlayspace().PlayerAddedEvent().Subscribe(PlayerProfileManager.InitializePlayer)
        PlayerProfileManager.InitializePlayers()
        for (Player : Self.GetPlayspace().GetPlayers()):
            UpdateStatusBillboard(Player)
            spawn{ RunStatusUpdateLoop(Player) }

    UpdateClass(Agent:agent):void= 
        PlayerProfileManager.UpdateClass(Agent)
        UpdateStatusBillboard(Agent)

    AddXPSmall(Agent:agent):void= 
        PlayerProfileManager.GrantXPSmall(Agent)
        UpdateStatusBillboard(Agent)

    AddXPMedium(Agent:agent):void= 
        PlayerProfileManager.GrantXPMedium(Agent)
        UpdateStatusBillboard(Agent)

    AddXPLarge(Agent:agent):void= 
        PlayerProfileManager.GrantXPLarge(Agent)
        UpdateStatusBillboard(Agent)

    CompleteQuest(Agent:agent):void= 
        PlayerProfileManager.CompleteQuest(Agent)
        UpdateStatusBillboard(Agent)

    ClearData(Agent:agent):void= 
        PlayerProfileManager.ClearPlayerProfile(Agent)
        UpdateStatusBillboard(Agent)
                    

    UpdateStatusBillboard(Agent:agent):void=
        if:
            PlayerInfo := PlayerProfileManager.GetPlayerProfileInfo[Agent]
        then:
            StatusBillboard.SetText(
                StatusMessage(
                    ActionCount:=PlayerInfo.ActionCount,
                    XP:=PlayerInfo.XP,
                    Mana:=PlayerInfo.Mana,
                    PlayerInfo.RankName,
                    PlayerInfo.CompletedQuestCount,
                    PlayerInfo.QuestHistoryEntryCount,
                    PlayerInfo.EventLogCount
                )
            )

    RunStatusUpdateLoop(Agent:agent)<suspends>:void =
        loop:
            UpdateStatusBillboard(Agent)
            Sleep(0.0)
