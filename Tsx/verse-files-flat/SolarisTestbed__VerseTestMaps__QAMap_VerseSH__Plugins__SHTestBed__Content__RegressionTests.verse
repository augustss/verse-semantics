# * Verse QA ShitHappens Epic https://jira.it.epicgames.com/browse/SOL-6087 * #
using { /Fortnite.com/VerseInternalTesting/VerseFramework }
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

regression_test_runner := class(test_runner_device):


regression_tests := class(test_device):
    SuiteName<override>:string = "RegressionTestSuite"

    TestCases<override>():[]test_case =
        array:
            test_case:
                Name := "NewCreativeDeviceGetPlayspace"
                Func := NewCreativeDeviceGetPlayspace
                Category := array{"editor"}

    # sh-02-07-fne-live-28-20-vk-stability-issues-ext
    # https://jira.it.epicgames.com/browse/SOL-6208
    NewCreativeDeviceGetPlayspace():void =
        TestRunner.CheckTrue(
            logic{GetPlayspace().GetPlayers() = creative_device{}.GetPlayspace().GetPlayers()},
            "GetPlayspace().GetPlayers()'s result must match regardless of creative_device")

# TODO: Wes - Make a test case in regression_tests that indicates this passed 
# sh-04-09-vk-play-server-mtbf-down-from-5-weeks-to-ext
# https://jira.it.epicgames.com/browse/SOL-6301
sol_5883 := class(creative_device):
    # prior to the bugfix, this used to crash UEFN when added to the map,
    # and crash the server whenever the map would start
    var X:int = 44
    block:
        set X = 45
