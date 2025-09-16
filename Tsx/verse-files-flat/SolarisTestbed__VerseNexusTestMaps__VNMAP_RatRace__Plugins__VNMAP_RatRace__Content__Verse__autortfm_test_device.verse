
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

var TestWeakMap:weak_map(session, int) = map{}

autortfm_test_device := class(creative_device):
    
    RunTest():void=
        var Session:?session = false
        
    ################################
    # GetSession in failed context #
    ################################
        if:
            set Session = option{GetSession()}
            set TestWeakMap[GetSession()] = 1
            true = false

        then:
            Print("NexusError - 'true = false' succeeded when it should fail in autortfm_test_device.RunTest()")

        else:
            var FirstFailed:logic = false

            if(TestWeakMap[GetSession()] = 1):
                set FirstFailed = true
                Print("NexusError - Failed to roll back change to TestWeakMap in a failure context in autortfm_test_device.RunTest()")
            
                if(not Session = false):
                    set FirstFailed = true
                    Print("NexusError - Failed to roll back change to Session variable in a failure context in autortfm_test_device.RunTest()")
            
                if(not FirstFailed = true):
                    Print("GetSession successfully rolled back!")
            
    ####################################
    # GetSession in successful context #
    ####################################
        if:
            set Session = option{GetSession()}
            set TestWeakMap[GetSession()] = 2
            true = true

        then:
            var SecondFailed:logic = false

            if(not TestWeakMap[GetSession()] = 2):
                set SecondFailed = true
                Print("NexusError - Failed to set key/value in TestWeakMap in autortfm_test_device.RunTest()")

            
            if(Session = false):
                set SecondFailed = true
                Print("NexusError - Failed to save session from GetSession to Session variable in autortfm_test_device.RunTest()")
            
            if(not SecondFailed = true):
                Print("GetSession test successful!")

        else:
            Print("NexusError - 'true = true' failed when it should succeed in autortfm_test_device.RunTest()")

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - autortfm_test_device")
        RunTest()
    
    OnEnd<override>():void=
        Print("Nexus OnEnd - autortfm_test_device")