using { /Fortnite.com/VerseInternalTesting/VerseFramework}
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }


test_runner_alias := class(test_runner_device){}


log_test_device := class(log_channel){}
test_monitor_device_log := class(log) { Channel<override>:subtype(log_channel) = log_test_device }

# Test monitor device watches the TestRunnerDevice and will trigger end game when the tests complete.
test_monitor_device<epic_internal> := class(creative_device):

    @editable
    TestRunnerDevice:?test_runner_device = false

    var InternalTestRunner:test_runner_device = test_runner_device{}

    @editable
    EndGameDevice:?end_game_device = false

    var InternalEndGameDevice:end_game_device = end_game_device{}

    OnBegin<override>()<suspends>:void =
        if (TestRunnerDeviceValid := TestRunnerDevice?):
            set InternalTestRunner = TestRunnerDeviceValid
        else:
            test_monitor_device_log{}.Print("test_monitor_device does not have a TestRunnerDevice assigned.", ?Level:=log_level.Error)

        if (EndGameDeviceValid := EndGameDevice?):
            set InternalEndGameDevice = EndGameDeviceValid
        else:
            test_monitor_device_log{}.Print("test_monitor_device does not have a EndGameDevice assigned.", ?Level:=log_level.Error)

        spawn{ RunTestMonitorLoop() }

    RunTestMonitorLoop()<suspends>:void=
        loop:
            if (InternalTestRunner.Status = test_runner_status.Complete):
                if:
                    Players := Self.GetPlayspace().GetPlayers()
                    Player := Players[0]
                    EndGameDeviceValid := EndGameDevice?
                then:
                    InternalTestRunner.TestRunner.Log(log_level.Normal, "InternalTestRunner.Status = Complete, Ending the minigame now")
                    InternalEndGameDevice.Activate(Player)
                else:
                    InternalTestRunner.TestRunner.Log(log_level.Error, "InternalTestRunner.Status = Complete, but we failed to reference the Player or end game device reference is null")
                break

            Sleep(1.0)      