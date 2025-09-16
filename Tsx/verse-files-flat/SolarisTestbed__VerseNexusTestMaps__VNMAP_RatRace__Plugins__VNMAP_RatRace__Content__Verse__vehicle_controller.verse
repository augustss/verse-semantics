
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Vehicles }
using { /Fortnite.com/Characters }
using { /Verse.org/Simulation/Tags }

# One of the objects with this tag is a dummy - it's a prop instead of a player spawner
PlayerSpawnerTag := class(tag){}

vehicle_controller := class(creative_device):
    @editable
    VehicleSpawner:vehicle_spawner_sports_car_device = vehicle_spawner_sports_car_device{}
    @editable
    VFXSpawner:vfx_spawner_device = vfx_spawner_device {}

    var CharacterMap:[agent]fort_character = map{}
    var MainVehicle:?fort_vehicle = false

    # Variables for AutoRTFM tests:
    var TestVehicle:?fort_vehicle = false
    var SubscribableTestFlag:logic = false
    
    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - vehicle_controller")
        PlayerSpawners := GetCreativeObjectsWithTag(PlayerSpawnerTag{})

        for(SpawnerList:PlayerSpawners, Spawner := player_spawner_device[SpawnerList]):
            #Print("Adding a Player Spawner to SpawnerList")
            Spawner.SpawnedEvent.Subscribe(OnPlayerSpawned)

        VehicleSpawner.AgentEntersVehicleEvent.Subscribe(TestGetVehicle)
        TestSubscribable()

        loop:
            Sleep(0.01)
            if(Vehicle := MainVehicle?):
                #Print("VFXSpawner following car")
                VFXSpawner.MoveTo(Vehicle.GetTransform().Translation, VFXSpawner.GetTransform().Rotation, 0.01)

            for(Key->Value:CharacterMap):

                if(Char:fort_character = CharacterMap[Key]):

                    if(Vehicle:fort_vehicle = Char.GetVehicle[]):
                        #Print("This player is in a vehicle!")
                    else:
                        #Print("No player vehicle found...")

                else:
                    #Print("Failed to find character!")

    
    OnPlayerSpawned(Agent:agent):void=
        if:
            #Print("Adding player to PlayerMap")
            set CharacterMap[Agent] = Agent.GetFortCharacter[]

    OnVehicleSpawned(Vehicle:fort_vehicle):void=
        set MainVehicle = option{Vehicle}
        set SubscribableTestFlag = true

##################################
# Functions for AutoRTFM Testing #
# SOL-5924, SOL-5922             #
##################################
    TestGetVehicle(Agent:agent):void=
        if:
            Char:fort_character = Agent.GetFortCharacter[]

        then:
        ################################
        # GetVehicle in failed context #
        ################################
            if:
                set TestVehicle = option{Char.GetVehicle[]}
                true = false

            then:
                Print("NexusError - 'true = false' succeeded when it should fail in vehicle_controller.TestGetVehicle()")

            else:
                if(not TestVehicle = false):
                    Print("NexusError - Failed to roll back setting TestVehicle to GetVehicle() result in vehicle_controller.TestGetVehicle()")
                
                else:
                    Print("GetVehicle successfully rolled back!")

        ####################################
        # GetVehicle in successful context #
        ####################################
            if:
                set TestVehicle = option{Char.GetVehicle[]}
                true = true

            then:
                if(TestVehicle = false):
                    Print("NexusError - Failed to set TestVehicle to GetVehicle() result in vehicle_controller.TestGetVehicle()")
                
                else:
                    Print("GetVehicle test successful!")

            else:
                Print("NexusError - 'true = true' failed when it should succeed in vehicle_controller.TestGetVehicle()")

        else:
            Print("NexusError - Failed to set Char to Agent.GetFortCharacter in vehicle_controller.TestGetVehicle()")

    TestSubscribable()<suspends>:void=
    ##################################
    # Subscribable in failed context #
    ##################################
        if:
            VehicleSpawner.SpawnedEvent.Subscribe(OnVehicleSpawned)
            true = false

        then:
            Print("NexusError - 'true = false' succeeded when it should fail in vehicle_controller.TestSubscribable()")

        else:
            if(SubscribableTestFlag = true):
                Print("NexusError - Failed to roll back subscription to VehicleSpawnedEvent in vehicle_controller.TestSubscribable()")

            else:
                Print("Subscription to VehicleSpawnedEvent successfully rolled back!")

            set SubscribableTestFlag = false
            VehicleSpawner.RespawnVehicle()
            Sleep(3.0)

    ######################################
    # Subscribable in successful context #
    ######################################
        if:
            VehicleSpawner.SpawnedEvent.Subscribe(OnVehicleSpawned)
            true = true

        then:
            if(SubscribableTestFlag = false):
                Print("NexusError - Failed to subscribe to VehicleSpawnedEvent in vehicle_controller.TestSubscribable()")

            else:
                Print("Subscribable test successful!")

        else:
            Print("NexusError - 'true = true' failed when it should succeed in vehicle_controller.TestSubscribable")

    
    OnEnd<override>():void=
        Print("Nexus OnEnd - vehicle_controller")