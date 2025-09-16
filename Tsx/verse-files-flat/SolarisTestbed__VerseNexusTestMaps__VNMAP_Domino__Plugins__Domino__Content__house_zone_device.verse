
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
using { /UnrealEngine.com/Temporary/SpatialMath }

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

# A Verse-authored creative device that can be placed in a level
house_zone_device := class(creative_device):

    @editable
    HouseTeleport : teleporter_device = teleporter_device{}

    @editable
    CharacterDevice : character_device = character_device{}

    @editable
    PerceptionTrigger : perception_trigger_device = perception_trigger_device{}

    @editable
    AirVent : air_vent_device = air_vent_device{}

    @editable
    TurretDevice : automated_turret_device = automated_turret_device{}

    @editable
    Bomb : explosive_device = explosive_device {}
    
    @editable
    Wildlife : wildlife_spawner_device = wildlife_spawner_device{}

    @editable
    ChairDevice : chair_device = chair_device{}

    @editable
    LockDevice : lock_device = lock_device{}

    @editable
    EndDevice : force_end_device = force_end_device{}

    ExpectedWildlifeSpawnAmount : int = 1;
    var WildlifeSpawnAmount : int = 0;

    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - house_zone_device")
        HouseTeleport.Enable()
        AirVent.Disable()

        Wildlife.Disable()
        HouseTeleport.TeleportedEvent.Subscribe(RunHouseTrip)
        PerceptionTrigger.DeviceSeesAgentEvent.Subscribe(EmptyEvent) #Empty event needs for correct work of event with trigger API
        TurretDevice.TargetLostEvent.Subscribe(BombTurret)
        Wildlife.SpawnedEvent.Subscribe(CheckWildlifeSpawnAmount)
        ChairDevice.ExitedEvent.Subscribe(OnChairExitedEvent)

    CheckWildlifeSpawnAmount(Agent : agent):void=
        Print("Check wildlife spawn amount")
        set WildlifeSpawnAmount += 1
        if (WildlifeSpawnAmount <> ExpectedWildlifeSpawnAmount):
            Print("NexusError - Amount of wildlife spawn should be {ExpectedWildlifeSpawnAmount} but got {WildlifeSpawnAmount} in CheckWildlifeSpawnAmount() of house_zone_device")    

    OnEnd<override>():void=
        Print("Nexus OnEnd - house_zone_device")

    EmptyEvent(Agent : agent):void=
        {}

    TeleportPlayer(Agent : agent):void=
        Print("Teleporting to house")
        HouseTeleport.Teleport(Agent)

    OnChairExitedEvent(Agent : agent):void=
        spawn{HouseZoneEnd(Agent)}

    HouseZoneEnd(Agent : agent)<suspends>:void=    
        Sleep(5.0)
        if(ChairDevice.IsOccupied[] and ChairDevice.IsSeated[Agent]):
            Print("NexusError - chair couldn't be occupied in OnChairExitedEvent() of house_zone_device")
        else:
            Print("Character moved from chair successfully")    

        EndDevice.ActivateEndGame()  
            
    ChairInteraction(Agent : agent):void=
        ChairDevice.Disable()
        ChairDevice.Seat(Agent)
        if(ChairDevice.IsOccupied[] and ChairDevice.IsSeated[Agent]):
            Print("NexusError - chair couldn't be occupied in ChairInteraction() of house_zone_device")
        
    RunHouseTrip(Agent : agent):void=
        CharacterDevice.PlayEmote()
        ChairInteraction(Agent)
        spawn{RunHouseTripSuspends(Agent)}

    RunHouseTripSuspends(Agent : agent)<suspends>:void=
        PerceptionTrigger.SetMaxTriggerCount(2)
        loop:
            Print("Amount of triggers: {PerceptionTrigger.GetTriggerCountRemaining()}")
            if (PerceptionTrigger.GetTriggerCountRemaining() = 0):
                AirVent.Activate()
                break
            Sleep(0.0) 
        PerceptionTrigger.Disable()    
        Wildlife.Spawn()
        Wildlife.Enable()
        TurretDevice.SetTarget(Agent)
        Wildlife.Spawn()   
        LockDevice.Open(Agent)            

    BombTurret(Agent : agent):void=        
        if (FortCharacter := Agent.GetFortCharacter[]):
            if (FortCharacter.TeleportTo[Wildlife.GetTransform().Translation, Wildlife.GetTransform().Rotation]):
            
            if (Bomb.TeleportTo[TurretDevice.GetTransform().Translation, TurretDevice.GetTransform().Rotation]):
                Bomb.Reset()
                ChairDevice.Enable()
                spawn{ChairBombInteraction(Agent)}

    ChairBombInteraction(Agent : agent)<suspends>:void=            
        ChairDevice.Seat(Agent)
        Sleep(3.0)
        if(ChairDevice.IsOccupied[] and ChairDevice.IsSeated[Agent]) {}
        else:
            Print("NexusError - chair must be occupied in ChairBombInteraction() of house_zone_device")
        Bomb.Explode(Agent)
                   