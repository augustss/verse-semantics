using { /Fortnite.com/Devices }
using { /Fortnite.com/Playspaces }
using { /Fortnite.com/Characters }
using { /Fortnite.com/AI }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

objective_device_gate := enum:
    EnableBeforeDestroyed
    EnableAfterDestroyed
    IgnoreObjective

ai_spawner := class<concrete>:
    @editable
    GuardSpawners:[]guard_spawner_device = array{}

    @editable
    NPCSpawnerBasic:npc_spawner_device = npc_spawner_device{}

    @editable
    NPCSpawnerEnhanced:npc_spawner_device = npc_spawner_device{}

    @editable
    Gate:objective_device_gate = objective_device_gate.IgnoreObjective

    @editable
    ObjectiveDevices:[]objective_device = array{}

    @editable
    InitialActivationDelay: float = 0.0

    @editable
    SpawnTimer: float = 5.0

    @editable
    RequiredPlayerCount:int = 0
    
    @editable
    BasicGuardsToSpawnPerWave:int = 3
    
    @editable
    EnhancedGuardsToSpawnPerWave:int = 1

    @editable
    MaxBasicGuards:int = 0

    @editable
    MaxEnhancedGuards:int = 0

    @editable
    MovementSpeedMultiplier:float = 1.0

    @editable
    DebugInfo:string = ""

    var ObjectivesDestroyed:int = 0
    var Enabled:logic = false
    var BasicGuardsCount:int = 0
    var EnhancedGuardsCount:int = 0
    var PrintLoggingEnabled:logic = false
    var EnhancedGuardsActive:[]fort_character = array{}
    var BasicGuardsActive:[]fort_character = array{}

    AllObjectivesDestroyed:event() = event(){}

    TextToPrint<localizes>(DebugInfoToPrint:string, Text:string): message = "{DebugInfoToPrint} - {Text}"

    PrintLogging(Text:string):void=
        if(PrintLoggingEnabled?):
            Print(TextToPrint(DebugInfo,Text))

    Enable(PrintLogs:logic):void=
        set PrintLoggingEnabled = PrintLogs

        for (GuardSpawner:GuardSpawners):
            GuardSpawner.SpawnedEvent.Subscribe(OnSpawnAdjustGuardSpeed)
        set Enabled = true

        NPCSpawnerBasic.SpawnedEvent.Subscribe(OnSpawnAdjustGuardSpeed)
        NPCSpawnerBasic.EliminatedEvent.Subscribe(OnBasicGuardEliminated)

        NPCSpawnerEnhanced.SpawnedEvent.Subscribe(OnSpawnTrackEnhancedGuard)
        NPCSpawnerEnhanced.EliminatedEvent.Subscribe(OnEnhancedGuardEliminated)
        
        spawn:
            StartSpawning()

    Disable():void=
        for (GuardSpawner:GuardSpawners):
            GuardSpawner.Disable()
        set Enabled = false

    WaitUntilObjectivesDestroyed()<suspends>:void=
        RegisterForObjectiveDestruction()
        AllObjectivesDestroyed.Await()

    RegisterForObjectiveDestruction():void=
        for (Device:ObjectiveDevices):
            Device.DestroyedEvent.Subscribe(OnObjectiveDestroyed)

    OnObjectiveDestroyed(Agent:agent):void=
        set ObjectivesDestroyed += 1
        if (ObjectivesDestroyed >= ObjectiveDevices.Length):
            AllObjectivesDestroyed.Signal()
    
    OnSpawnAdjustGuardSpeed(Agent:agent):void=
        if (FortCharacter := Agent.GetFortCharacter[]):

            set BasicGuardsActive = BasicGuardsActive + array{FortCharacter}
            if(NavigatableInterface := FortCharacter.GetNavigatable[]):
                NavigatableInterface.SetMovementSpeedMultiplier(MovementSpeedMultiplier)
        
            PrintLogging("Adjusting Guard speed and unleashing")
    
    OnSpawnTrackEnhancedGuard(Agent:agent):void=
        if (FortCharacter := Agent.GetFortCharacter[]):
            set EnhancedGuardsActive = EnhancedGuardsActive + array{FortCharacter}

    OnEnhancedGuardEliminated(InteractionResult:device_ai_interaction_result):void=

        if (Agent:=InteractionResult.Target?, FortCharacter := Agent.GetFortCharacter[], ActiveUpdate:= EnhancedGuardsActive.RemoveFirstElement[FortCharacter]):
            set EnhancedGuardsActive = ActiveUpdate
            set EnhancedGuardsCount = Max(EnhancedGuardsActive.Length, 0)

        PrintLogging("Enhanced Guard Dead, Total {EnhancedGuardsCount+BasicGuardsCount}")

    OnBasicGuardEliminated(InteractionResult:device_ai_interaction_result):void=
        if (Agent:=InteractionResult.Target?, FortCharacter := Agent.GetFortCharacter[], ActiveUpdate:= BasicGuardsActive.RemoveFirstElement[FortCharacter]):
            set BasicGuardsActive = ActiveUpdate
            set BasicGuardsCount = Max(BasicGuardsActive.Length, 0)
        PrintLogging("Basic Guard Dead, Total {EnhancedGuardsCount+BasicGuardsCount}")

    StartSpawner(PrintLogs:logic, Playspace:fort_playspace)<suspends>:void=
        if (Gate = objective_device_gate.EnableAfterDestroyed):
            Disable()
            WaitUntilObjectivesDestroyed()

        Sleep(InitialActivationDelay)

        if (Playspace.GetPlayers().Length >= RequiredPlayerCount):
            Enable(PrintLogs)
            if (Gate = objective_device_gate.EnableBeforeDestroyed):
                WaitUntilObjectivesDestroyed()
                Disable()
        else:
            Disable()
    
    StartSpawning()<suspends>:void=
        loop:
            # for (GuardSpawner:GuardSpawners):
            #     GuardSpawner.Spawn()

            if (BasicGuardsToSpawnPerWave > 0):
                var ToSpawn:int = Min(BasicGuardsToSpawnPerWave, MaxBasicGuards-BasicGuardsCount)
                if (ToSpawn > 0):
                    for(Count:=1..ToSpawn):
                        set BasicGuardsCount += 1
                        PrintLogging("Spawning Basic {Count}, RunningTotal Is {BasicGuardsCount}")
                        NPCSpawnerBasic.Enable() # Calling Enable is effectively like calling Spawn() and doing this way avoids an extra spawned guard
                        Sleep(0.01) 
                        
            if (EnhancedGuardsToSpawnPerWave > 0):
                var ToSpawn:int = Min(EnhancedGuardsToSpawnPerWave, MaxEnhancedGuards-EnhancedGuardsCount)
                if (ToSpawn > 0):
                    for(Count:=1..ToSpawn):
                        set EnhancedGuardsCount += 1
                        PrintLogging("Spawning Enhanced {Count}, RunningTotal Is {EnhancedGuardsCount}")
                        NPCSpawnerEnhanced.Enable() # Calling Enable is effectively like calling Spawn() and doing this way avoids an extra spawned guard
                        Sleep(0.01)
                        
            # Sleep between waves
            Sleep(SpawnTimer)
            PrintLogging("Starting next wave loop")

            if(not Enabled?):
                break

ai_wave_spawner := class(creative_device):

    @editable
    Spawners:[]ai_spawner = array{}
    
    @editable
    PrintLoggingEnabled:logic = false

    #TODO: Add support for more fine tuned spawning control

    OnBegin<override>()<suspends>:void=
        StartSpawners()

    StartSpawners()<suspends>:void=
        Playspace := GetPlayspace()
        for(Spawner:Spawners):
            spawn { Spawner.StartSpawner(PrintLoggingEnabled, Playspace) }
