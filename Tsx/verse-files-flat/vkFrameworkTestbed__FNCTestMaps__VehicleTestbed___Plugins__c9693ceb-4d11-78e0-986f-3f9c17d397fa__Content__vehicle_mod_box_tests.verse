using { /Fortnite.com/Devices }
using { /Fortnite.com/Vehicles }
using { /Fortnite.com/VerseInternalTesting/NexusFramework }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation }

nexus_test_runner_alias := class<epic_internal>(nexus_test_runner){}

vehicle_mod_box_tests := class(nexus_test_device):
    @editable
    ModBox:vehicle_mod_box_spawner_device = vehicle_mod_box_spawner_device{}
    @editable
    PlayerSpawner:player_spawner_device = player_spawner_device{}
    @editable
    TriggerArray:[]trigger_device = array{trigger_device{}}
    @editable
    SUVSpawner:vehicle_spawner_valet_suv_device = vehicle_spawner_valet_suv_device{}
    @editable
    EndGame:round_settings_device = round_settings_device{}
    @editable
    EndGameTimer: timer_device = timer_device{}

    var LastTestIndex:int = 0
    var DestroyedBoxCounter:int = 0
    var ModAppliedCounter:int = 0
    var TeleportAmount:int = 5
    var InitialGridX:float = -512.0
    var InitialGridY:float = -512.0
    var AgentEnteredVehicle:logic = false

    OnBegin<override>()<suspends>:void=
        (super:)OnBegin()
        if(TriggerArray[0].TriggeredEvent.Subscribe(StartModBoxCycleTests)){}

        for (Index := 1..5):    
            if(TriggerArray[Index].TriggeredEvent.Subscribe(ModBoxCycleTests)){}

        PlayerSpawner.SpawnedEvent.Subscribe(PlayerSpawnedEvent)
        ModBox.BoxDestroyedEvent.Subscribe(DestroyedModBox)
        EndGameTimer.SuccessEvent.Subscribe(EndOfGame)
        SUVSpawner.AgentEntersVehicleEvent.Subscribe(EnteredVehicle)


    PlayerSpawnedEvent(Agent:agent):void=
        IsModBoxEnabled()
        SUVSpawner.AssignDriver(Agent)
        MoveModBox(Position := vector3{X:=InitialGridX*2})

    IsModBoxEnabled():void=
        if(ModBox.IsEnabled[]):
            Print("Vehicle Mod Box is enabled")
        else:
            Print("Vehicle Mod Box is disabled")

    StartModBoxCycleTests(Agent:?agent):void=
        ModBox.CycleToNextValidIndex()
        set LastTestIndex = ModBox.GetCurrentIndex()
        Print("Current Mod Index = {LastTestIndex}")


    ModBoxCycleTests(Agent:?agent):void=
        TeleportBox()
        set LastTestIndex = ModBox.GetCurrentIndex()
        Print("Current Mod Index = {LastTestIndex}")

    DestroyedModBox(Agent:?agent, CurrentModIndex:int):void=
        set DestroyedBoxCounter += 1
        ModBox.ModAppliedEvent.Subscribe(ModAppliedEvent)

        AddCheck(logic{CurrentModIndex = LastTestIndex}, "Mod Box Index", "Testing if CurrentModIndex ({CurrentModIndex}) is equal to LastIndex ({LastTestIndex}) when destroying the mod box")
        
        if(DestroyedBoxCounter = 6):
            Print("Reset DestroyedBoxCounter back to 0")
            if(VehicleAgent := Agent?):
                spawn{EnterVehicle(VehicleAgent)}
            set DestroyedBoxCounter = 0


    ModAppliedEvent(Agent:?agent, Vehicle:fort_vehicle, ModInt:int):void=
        set ModAppliedCounter += 1
        Print("Mod Applied Success = {ModInt}")
        Print("Mod Applied Counter = {ModAppliedCounter}")


    MoveModBox(Position:vector3):void=
        AddCheck(logic{ModBox.TeleportTo[Position, Rotation := rotation{}]}, "Teleport Mod Box", "Testing if the mod box was teleported to a position")

    TeleportBox():void=
        ModBox.DespawnBox()
        MoveModBox(Position := vector3{X:=InitialGridX*TeleportAmount})
        set TeleportAmount += 3
        ModBox.CycleToNextValidIndex()
        ModBox.SpawnBox()

    EnterVehicle(Agent:agent)<suspends>:void=
        SUVSpawner.DestroyVehicle()

    EndOfGame(Agent:?agent):void=
        if (MyAgent := Agent?):
            EndGame.EndRound(MyAgent)

    EnteredVehicle(Agent:agent):void=
        set AgentEnteredVehicle = true
        AddCheck(AgentEnteredVehicle, "Agent Enters Vehicle", "Testing if the agent successfully entered a vehicle")

    OnEnd<override>():void=
        (super:)OnEnd() 
