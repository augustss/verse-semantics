# Copyright Epic Games, Inc. All Rights Reserved.

using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }

verse_persistence_test_device<public> := class(creative_device):

    @editable
    PlayerProfileManager:player_profile_manager_device = player_profile_manager_device{}

    @editable
    AnalyticsBillboard:billboard_device = billboard_device{}

    @editable
    TimingsBillboard:billboard_device = billboard_device{}


    @editable
    StartLoadTestButton:button_device = button_device{}

    @editable
    StopLoadTestButton:button_device = button_device{}


    @editable
    AddQuestCompleteLoadTestButton:button_device = button_device{}

    @editable
    RemoveQuestCompleteLoadTestButton:button_device = button_device{}

    @editable
    IncreaseQuestCompleteOpsRateButton:button_device = button_device{}

    @editable
    ResetQuestCompleteOpsRateButton:button_device = button_device{}


    @editable
    AddGrantXpLoadTestButton:button_device = button_device{}

    @editable
    RemoveGrantXpLoadTestButton:button_device = button_device{}

    @editable
    IncreaseGrantXpOpsRateButton:button_device = button_device{}

    @editable
    ResetGrantXpOpsRateButton:button_device = button_device{}


    @editable
    RemoveAllLoadTestsButton:button_device = button_device{}

    QuestCompleteLoadTest:persistence_load_test_quest_complete = persistence_load_test_quest_complete{}
    GrantXpLoadTest:persistence_load_test_grant_xp = persistence_load_test_grant_xp{}

    # test configs
    LoadTestRunner:persistence_load_test_runner = persistence_load_test_runner{}

    OnBegin<override>():void =
        StartLoadTestButton.InteractedWithEvent.Subscribe(StartLoadTest)
        StopLoadTestButton.InteractedWithEvent.Subscribe(StopLoadTest)

        AddQuestCompleteLoadTestButton.InteractedWithEvent.Subscribe(AddQuestCompleteLoadTest)
        RemoveQuestCompleteLoadTestButton.InteractedWithEvent.Subscribe(RemoveQuestCompleteLoadTest)
        IncreaseQuestCompleteOpsRateButton.InteractedWithEvent.Subscribe(IncreaseQuestCompleteOpsRate)
        ResetQuestCompleteOpsRateButton.InteractedWithEvent.Subscribe(ResetQuestCompleteOpsRate)

        AddGrantXpLoadTestButton.InteractedWithEvent.Subscribe(AddGrantXpLoadTest)
        RemoveGrantXpLoadTestButton.InteractedWithEvent.Subscribe(RemoveGrantXpLoadTest)
        IncreaseGrantXpOpsRateButton.InteractedWithEvent.Subscribe(IncreaseGrantXpOpsRate)
        ResetGrantXpOpsRateButton.InteractedWithEvent.Subscribe(ResetGrantXpOpsRate)

        RemoveAllLoadTestsButton.InteractedWithEvent.Subscribe(RemoveAllLoadTests)

        PlayerProfileManager.SetAnalytics(player_profile_manager_analytics{AnalyticsBillboard:=AnalyticsBillboard})
        set LoadTestRunner.PlayerProfileManager = PlayerProfileManager
        set LoadTestRunner.TimingsBillboard = TimingsBillboard

        spawn{ PlayerProfileManager.Analytics.RunUpdateLoop() }

            
    StartLoadTest(Agent:agent):void= 
        Print("Start LoadTestRunner...", ?Duration:=6.0)
        spawn{ LoadTestRunner.RunLoadTest(Agent, Self) }
        UpdateAnalytics()
                    
    StopLoadTest(Agent:agent):void= 
        Print("Stop GoldenPath load test...", ?Duration:=6.0)
        set LoadTestRunner.StopLoadTest = true


    AddQuestCompleteLoadTest(Agent:agent):void= 
        LoadTestRunner.AddLoadTest(QuestCompleteLoadTest)

    RemoveQuestCompleteLoadTest(Agent:agent):void= 
        LoadTestRunner.RemoveLoadTest(QuestCompleteLoadTest)

    IncreaseQuestCompleteOpsRate(Agent:agent):void= 
        if (QuestCompleteLoadTest.OperationsPerFrame = 1):
            set QuestCompleteLoadTest.OperationsPerFrame = 10
        else:
            set QuestCompleteLoadTest.OperationsPerFrame += 10
        UpdateAnalytics()
                    
    ResetQuestCompleteOpsRate(Agent:agent):void= 
        set QuestCompleteLoadTest.OperationsPerFrame = 1
        UpdateAnalytics()
                    
    
    AddGrantXpLoadTest(Agent:agent):void= 
        LoadTestRunner.AddLoadTest(GrantXpLoadTest)

    RemoveGrantXpLoadTest(Agent:agent):void= 
        LoadTestRunner.RemoveLoadTest(GrantXpLoadTest)

    IncreaseGrantXpOpsRate(Agent:agent):void= 
        if (GrantXpLoadTest.OperationsPerFrame = 1):
            set GrantXpLoadTest.OperationsPerFrame = 10
        else:
            set GrantXpLoadTest.OperationsPerFrame += 10
        UpdateAnalytics()
                    
    ResetGrantXpOpsRate(Agent:agent):void= 
        set GrantXpLoadTest.OperationsPerFrame = 1
        UpdateAnalytics()
                    
    RemoveAllLoadTests(Agent:agent):void= 
        LoadTestRunner.RemoveAllLoadTests()
                    
    
    UpdateAnalytics():void=
        set PlayerProfileManager.Analytics.QuestCompleteRate = QuestCompleteLoadTest.OperationsPerFrame
        set PlayerProfileManager.Analytics.GrantXpRate = GrantXpLoadTest.OperationsPerFrame
        PlayerProfileManager.Analytics.UpdateAnalyticsBillboard()
                    

