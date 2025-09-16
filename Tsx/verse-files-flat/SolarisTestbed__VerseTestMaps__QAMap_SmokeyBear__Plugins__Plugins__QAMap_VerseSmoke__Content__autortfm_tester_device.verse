
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Colors }
using { /Fortnite.com/Vehicles }
using { /Fortnite.com/Characters }

var RTFMSessionWeakMap:weak_map(session, int) = map{}

autortfm_tester_device := class(creative_device):
    @editable
    VehicleSpawner:vehicle_spawner_sports_car_device = vehicle_spawner_sports_car_device{}

    #var TestVehicle:?fort_vehicle = false
    var SubscribableTestFlag:logic = false
    
    GetSessionTest():void=
        var Session:?session = false
        
    ################################
    # GetSession in failed context #
    ################################
        if:
            set Session = option{GetSession()}
            set RTFMSessionWeakMap[GetSession()] = 1
            true = false

        then:
            Print("[FAILURE] 'true = false' succeeded when it should fail. | GetSessionTest()", ?Color := NamedColors.Red)

        else:
            var FirstFailed:logic = false

            if(RTFMSessionWeakMap[GetSession()] = 1):
                set FirstFailed = true
                Print("[FAILURE] Failed to roll back change to RTFMSessionWeakMap in a failure context. | GetSessionTest()", ?Color := NamedColors.Red)
            
                if(not Session = false):
                    set FirstFailed = true
                    Print("[FAILURE] Failed to roll back change to Session variable in a failure context. | GetSessionTest()", ?Color := NamedColors.Red)
            
                if(not FirstFailed = true):
                    Print("[SUCCESS] GetSession successfully rolled back. | GetSessionTest()", ?Color := NamedColors.LightGreen)
            
    ####################################
    # GetSession in successful context #
    ####################################
        if:
            set Session = option{GetSession()}
            set RTFMSessionWeakMap[GetSession()] = 2
            true = true

        then:
            var SecondFailed:logic = false

            if(not RTFMSessionWeakMap[GetSession()] = 2):
                set SecondFailed = true
                Print("[FAILURE] Failed to set key/value in RTFMSessionWeakMap. | GetSessionTest()", ?Color := NamedColors.Red)

            
            if(Session = false):
                set SecondFailed = true
                Print("[FAILURE] Failed to save session from GetSession to Session variable. | GetSessionTest()", ?Color := NamedColors.Red)
            
            if(not SecondFailed = true):
                Print("[SUCCESS] GetSession successfully completed. | GetSessionTest()", ?Color := NamedColors.LightGreen)

        else:
            Print("[FAILURE] 'true = true' failed when it should succeed. | GetSessionTest()", ?Color := NamedColors.Red)

    SubscribableTest()<suspends>:void=
    ##################################
    # Subscribable in failed context #
    ##################################
        if:
            VehicleSpawner.SpawnedEvent.Subscribe(OnVehicleSpawned)
            true = false

        then:
            Print("[FAILURE] 'true = false' succeeded when it should fail. | SubscribableTest()", ?Color := NamedColors.Red)

        else:
            if(SubscribableTestFlag = true):
                Print("[FAILURE] Failed to roll back subscription to VehicleSpawnedEvent. | SubscribableTest()", ?Color := NamedColors.Red)

            else:
                Print("[SUCCESS] Subscription to VehicleSpawnedEvent successfully rolled back. | SubscribableTest()", ?Color := NamedColors.LightGreen)

            set SubscribableTestFlag = false

    ######################################
    # Subscribable in successful context #
    ######################################
        if:
            VehicleSpawner.SpawnedEvent.Subscribe(OnVehicleSpawned)
            true = true

        then:
            Sleep(3.0)
            VehicleSpawner.RespawnVehicle()

            if(SubscribableTestFlag = false):
                Print("[FAILURE] Failed to subscribe to VehicleSpawnedEvent. | SubscribableTest()", ?Color := NamedColors.Red)

            else:
                Print("[SUCCESS] Successfully subscribed to VehicleSpawnedEvent. | SubscribableTest()", ?Color := NamedColors.LightGreen)

        else:
            Print("[FAILURE] 'true = true' failed when it should succeed. | SubscribableTest()", ?Color := NamedColors.Red)

    OnVehicleSpawned(Vehicle:fort_vehicle):void=
        set SubscribableTestFlag = true

    OnBegin<override>()<suspends>:void=
        Print("OnBegin autortfm_tester_device")
        GetSessionTest()
        SubscribableTest()
    
    OnEnd<override>():void=
        Print("OnEnd autortfm_tester_device")