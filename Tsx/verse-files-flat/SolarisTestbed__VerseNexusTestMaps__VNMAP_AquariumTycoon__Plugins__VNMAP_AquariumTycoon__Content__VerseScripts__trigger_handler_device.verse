
using { /Fortnite.com/Characters }
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /Verse.org/Simulation/Tags }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Random }
using { /Fortnite.com/VerseInternalTesting/NexusFramework}

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

device_trigger := class(tag){}

# A Verse-authored creative device that can be placed in a level
trigger_handler_device := class(nexus_test_device):

    #subscribe trigger events here
    #set up triggers with verse markup tags
    #marine_animals will spawn in and find a trigger
    #set off the trigger to call events on these devices.

    #Spawn Player
    @editable
    Spawners : []player_spawner_device = array{}

    #Devices
    @editable
    ExplosiveBarrel : explosive_device = explosive_device{}

    @editable
    WildLife : wildlife_spawner_device = wildlife_spawner_device{}

    #Triggers
    @editable
    DeviceTriggers : []trigger_device = array{}

    #Triggers
    @editable
    ExplosiveTrigger : trigger_device = trigger_device{}

    @editable
    Beacons : []beacon_device = array{}

    # Tests<override>():[]nexus_test= array:
    #     MakeNexusTest("Trigger Device")
    #     MakeNexusTest("Teleport Barrel")
    #     MakeNexusTest("Get Beacon from Array")

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - trigger_handler_device")

        for(playerspawn : Spawners):
            playerspawn.SpawnedEvent.Subscribe(OnPlayerSpawn)

        for(Trigger : DeviceTriggers):
            Trigger.TriggeredEvent.Subscribe(SuccessfulTrigger)
        

        (super:)OnBegin() # * All devices will need this line

        # SetupTest("Trigger Device", false)
        # SetupTest("Teleport Barrel", false)
        # SetupTest("Get Beacon from Array", false)
        # CompleteTest("Trigger Device", "Trigger Device Test Completed in OnBegin()", false)
        # CompleteTest("Teleport Barrel", "Teleport Barrel Test Completed in OnBegin()", false)
        # CompleteTest("Get Beacon from Array", "Get Beacon Test Completed in OnBegin()", false)

        
        DisableBeacons()
        WildLife.SpawnedEvent.Subscribe(EnableBeacon)

    OnEnd<override>():void =
        Print("Nexus OnEnd - trigger_handler_device")
        (super:)OnEnd() # * All devices will need this line
        
    OnPlayerSpawn(InAgent : agent): void =
        Print("Trigger Handler - OnPlayerSpawned is called")
        spawn{ExplodeBarrelLoop(InAgent)}

    DisableBeacons(): void =
        for(Beacon : Beacons):
            Beacon.Disable()

    EnableBeacon(NewAnimal : agent): void =
        RandomIndex : int = GetRandomInt(0, Beacons.Length - 1)
        if(ChosenBeacon := Beacons[RandomIndex]):
            AddPass("Get Beacon from Array", "Got beacon from array in EnableBeacon()")
            ChosenBeacon.Enable()
        else:
            AddFail("Get Beacon from Array", "Could not get beacon from array in EnableBeacon()")

    OutOfRangeTest() : void =
        Print("")

    SuccessfulTrigger(OptionalAgent : ?agent): void = 
        #Print("Triggered Device Trigger")
        AddPass("Trigger Device", "Trigger device was activated and called Successful Trigger()")

    ExplodeBarrel(Agent: agent): void =
        #Print("Exploding Barrel")
        ExplosiveBarrel.Explode(Agent)
        ExplosiveBarrel.Reset()

    ExplodeBarrelLoop(Agent : agent)<suspends>: void =
        Print("Spawned loop")
        loop:
            #Teleport to new location, based off tanks
            TaggedTanks := GetCreativeObjectsWithTag(aquariumtank{})
            RandomIndex : int = GetRandomInt(0, TaggedTanks.Length - 1)
            if(NewTankLoc := TaggedTanks[RandomIndex].GetTransform()):
                if(ExplosiveBarrel.TeleportTo[NewTankLoc]):
                    #Print("Teleport To Worked")
                    AddPass("Teleport Barrel", "Teleported barrel in ExplodeBarrelLoop()")
                    Sleep(0.2)
                else:
                    #Print("Teleport To did not Work")
                    AddFail("Teleport Barrel", "Unable to teleport barrel in ExplodeBarrelLoop()")
            ExplodeBarrel(Agent)
            Sleep(0.2)