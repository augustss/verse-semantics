# Copyright Epic Games, Inc. All Rights Reserved.

using { /Fortnite.com/Devices }
using { /Verse.org/Random }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

persistence_load_test_log := class(log_channel){}

timing_data := class:
    Name:string = ""
    var ReportingCount:int = 0
    var TotalTime:float = 0.0
    var MaxTimeLastMinute:float = 0.0
    var MaxTime:float = 0.0

    GetAverageTimeString():string=
        AverageTime := TotalTime / (ReportingCount * 1.0)
        GetDisplayTime(AverageTime)

    GetMaxTimeLastMinuteTimeString():string=
        GetDisplayTime(MaxTimeLastMinute)

    GetMaxTimeTimeString():string=
        GetDisplayTime(MaxTime)

    ResetForMinuteHeartbeat():void=
        set ReportingCount = 0
        set TotalTime = 0.0
        set MaxTimeLastMinute = 0.0

    RecordTime(Time:float):void=
        set ReportingCount += 1
        set TotalTime += Time
        if (Time > MaxTimeLastMinute):
            set MaxTimeLastMinute = Time
        if (Time > MaxTime):
            set MaxTime = Time

    GetDisplayTime<public>(DurationInSec:float):string =
        if (DurationInSec = Inf):
            return ""
        if (DurationInSec >= 60.0):
            return " (" + ToString(DurationInSec/60.0) + "m)"
        if (DurationInSec*1000 < 0.0):
            return " (" + ToString(DurationInSec*1000.0) + "ms)"
        return " (" + ToString(DurationInSec) + "s)"
        
    GetReportString():string=
        TimeAvg:string = GetAverageTimeString()
        TimeMaxLastMinute:string = GetMaxTimeLastMinuteTimeString()
        TimeMax:string = GetMaxTimeTimeString()
        "{Name}: {TimeAvg}|{TimeMaxLastMinute}|{TimeMax}"

        

persistence_load_test_base<public> := class:
    Name:string = ""
    TimingData:timing_data = timing_data{}

    RunTest(Agent:agent, PlayerProfileManager:player_profile_manager_device)<suspends>:void=
        TestStartTime := GetSimulationElapsedTime()
        RunTestInner(Agent, PlayerProfileManager)
        Sleep(0.0)
        TestEndTime := GetSimulationElapsedTime()
        TimingData.RecordTime(TestEndTime - TestStartTime)

    RunTestInner(Agent:agent, PlayerProfileManager:player_profile_manager_device):void={}

persistence_load_test_grant_xp<public> := class(persistence_load_test_base):
    Name<override>:string = "GrantXp"
    TimingData<override>:timing_data = timing_data{Name:="GrantXp"}
    var OperationsPerFrame:int = 1

    RunTestInner<override>(Agent:agent, PlayerProfileManager:player_profile_manager_device):void=
        for(Iter:=1..OperationsPerFrame):
            NewXp:int = GetRandomInt(10, 1000)
            PlayerProfileManager.GrantXP(Agent, NewXp)

persistence_load_test_quest_complete<public> := class(persistence_load_test_base):
    Name<override>:string = "QuestComplete"
    TimingData<override>:timing_data = timing_data{Name:="QuestComplete"}
    var OperationsPerFrame:int = 1

    RunTestInner<override>(Agent:agent, PlayerProfileManager:player_profile_manager_device):void=
        for(Iter:=1..OperationsPerFrame):
            PlayerProfileManager.CompleteQuest(Agent)

persistence_load_test_runner<public> := class:

    PersistenceLoadTestLog:log = log{Channel := persistence_load_test_log}

    var TimingsBillboard:billboard_device = billboard_device{}

    var PlayerProfileManager:player_profile_manager_device = player_profile_manager_device{}

    var StopLoadTest<public>:logic = false

    var LoadTests:[]persistence_load_test_base = array{}

    QuestCompleteTimingData:timing_data = timing_data{}
    GrantXpTimingData:timing_data = timing_data{}
    TotalTimingData:timing_data = timing_data{Name:="TOTAL"}

    TimingsMessage<localizes>(
        InTimeReport:string):message =
            "Timing avg|max(1min)|max:\n{InTimeReport}"

    AddLoadTest(NewTest:persistence_load_test_base):void=
        set LoadTests += array{NewTest}

    RemoveLoadTest(TestToRemove:persistence_load_test_base):void=
        set LoadTests = for:
            LoadTest:LoadTests
            not LoadTest.Name = TestToRemove.Name
        do:
            LoadTest

    RemoveAllLoadTests():void=
        set LoadTests = array{}

    RunLoadTest(Agent:agent, TestDevice:verse_persistence_test_device)<suspends>:void=
        set StopLoadTest = false

        race:
            loop:
                # run inner loop to do a bunch of stuff
                TotalStartTime := GetSimulationElapsedTime()

                for (LoadTest:LoadTests):
                    LoadTest.RunTest(Agent, PlayerProfileManager)

                Sleep(0.0)

                TotalEndTime := GetSimulationElapsedTime()
                TotalTimingData.RecordTime(TotalEndTime - TotalStartTime)

                #TestDevice.UpdateStatusBillboard(Agent)

                Sleep(0.0)

                if (StopLoadTest?):
                    break
            loop:
                # run inner loop to do a bunch of stuff
                #TestDevice.UpdateStatusBillboard(Agent)
                UpdateTimingBillboard()
                Sleep(1.0)
            loop:
                # reset the timing data on a 1 minute heartbeat
                for (LoadTest:LoadTests):
                    LoadTest.TimingData.ResetForMinuteHeartbeat()
                TotalTimingData.ResetForMinuteHeartbeat()
                Sleep(60.0)

    UpdateTimingBillboard():void=
        var TimeReport:string = ""
        for (LoadTest:LoadTests):
            CurTimeReport:string = LoadTest.TimingData.GetReportString()
            set TimeReport += "  {CurTimeReport}\n"

        TotalTimeReport:string = TotalTimingData.GetReportString()
        set TimeReport += "  {TotalTimeReport}\n"

        TimingsBillboard.SetText(TimingsMessage(InTimeReport:=TimeReport))