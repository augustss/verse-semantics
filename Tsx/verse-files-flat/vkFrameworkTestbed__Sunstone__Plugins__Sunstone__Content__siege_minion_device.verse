
using { /Fortnite.com/Devices }
using { /Fortnite.com/Characters }
using { /Fortnite.com/AI }
using { /Fortnite.com/Game }
using { /Verse.org/Simulation }
using { /Verse.org/Random }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

# A Verse-authored creative device that can be placed in a level
siege_minion_device := class(creative_device):
    @editable
    NPCMinionSpawners:[]npc_spawner_device = array{}

    @editable
    SecondsBetweenSpawns:float = 60.0

    @editable
    FinalPushSecondsBetweenSpawns:float = 30.0

    @editable
    ActivationDelay:float = 60.0
    
    @editable
    MovementSpeedMultiplier:float = 0.5

    @editable
    HealthAuraEffectRange:float = 20.0

    @editable
    HealthAuraHealAmount:float = 2.0

    @editable
    HealthAuraHealFrequency:float = 1.0

    @editable
    HealVFX:vfx_creator_device = vfx_creator_device{}

    @editable
    OnlyHealPlayers:logic = false

    @editable
    MinionSpawnEventToRelease:sub_events = sub_events.Team1SiegeMinionReleased

    @editable
    SentriesToListenForDestructionOf:[]sentry_device = array{}

    @editable
    PrintLoggingEnabled:logic = true

    @editable
    SiegeSpawnerDebugInfo:string = "Siege minion Spawner"

    var LastIndexSpawned:int = -1;
    var ActiveSiegeMinions:[]agent = array{}
    var IsRunning:logic = false
    var SentriesDestroyed:int = 0
    var AgentVFX:[]agent = array{}
    var CurrentSecondsBetweenSpawns:float = 30.0

    TextToPrint<localizes>(DebugInfoToPrint:string, Text:string): message = "{DebugInfoToPrint} - {Text}"

    PrintLogging(Text:string):void=
        if(PrintLoggingEnabled?):
            Print(TextToPrint(SiegeSpawnerDebugInfo,Text))

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        set CurrentSecondsBetweenSpawns = SecondsBetweenSpawns
        for (Spawner:NPCMinionSpawners):
            Spawner.SpawnedEvent.Subscribe(OnSpawnAdjustSpeed)

        for (Sentry:SentriesToListenForDestructionOf):
            Sentry.EliminatedEvent.Subscribe(OnSentryDestroyed)

        spawn:
            StartSpawning()

    OnSentryDestroyed(Eliminator:?agent):void=
        set SentriesDestroyed = SentriesDestroyed + 1

        PrintLogging("Sentry Destroyed! Count at {SentriesDestroyed}")
        if(SentriesDestroyed = SentriesToListenForDestructionOf.Length):
            PrintLogging("Set Spawn time to {FinalPushSecondsBetweenSpawns}")
            set CurrentSecondsBetweenSpawns = FinalPushSecondsBetweenSpawns 
            # faster at end of game

    StartSpawning()<suspends>:void=
        Sleep(ActivationDelay)

        loop:
            Sleep(CurrentSecondsBetweenSpawns)
            
            var SpawnerCount:int = NPCMinionSpawners.Length

            if (SpawnerCount > 0):
                PrintLogging("Have {SpawnerCount} spawners, picking one")
                if:
                    SentriesToDestroy:= SentriesToListenForDestructionOf.Length
                    SentriesDestroyed < SentriesToDestroy or SentriesToDestroy = 0
                then:
                    # pick a spawner, spawn, record which
                    var SpawnIndex:int = GetRandomInt(0, SpawnerCount-1)
                    # increase or wrap if it was the same as last time
                    if (SpawnIndex = LastIndexSpawned, NewIndex:= SpawnIndex+1):
                        if(NewIndex >= SpawnerCount):
                            set SpawnIndex = 0
                        else:
                            set SpawnIndex = NewIndex

                    if(Spawner:=NPCMinionSpawners[SpawnIndex]):
                        SendUIEvent()
                        Spawner.Enable()
                        
                        PrintLogging("Spawned from {SpawnIndex}")
                        set LastIndexSpawned = SpawnIndex
                else:
                    # Final push! Spawn them all!
                    
                    PrintLogging("Spawn from all Spawners")
                    SendUIEvent()
                    for(Spawner:NPCMinionSpawners):
                        Spawner.Enable()
    
    SendUIEvent():void=
        if (GEM := global_events_accessor{}.Instance[Self]):
            GEM.TriggerEvent(event_type.Base, MinionSpawnEventToRelease)    

    OnSiegeMinionEliminated(EliminationResult:elimination_result):void=
        if(Agent:= EliminationResult.EliminatedCharacter.GetAgent[], Agents := ActiveSiegeMinions.RemoveAllElements(Agent)):
            set ActiveSiegeMinions = Agents

            RemoveAgentFromHealVFXTracker(Agent)

            if(ActiveSiegeMinions.Length = 0):
                set IsRunning = false

    OnSpawnAdjustSpeed(Agent:agent):void=
        if (FortCharacter := Agent.GetFortCharacter[]):

            if(NavigatableInterface := FortCharacter.GetNavigatable[]):
                NavigatableInterface.SetMovementSpeedMultiplier(MovementSpeedMultiplier)

            set ActiveSiegeMinions += array{Agent}
            FortCharacter.EliminatedEvent().Subscribe(OnSiegeMinionEliminated)
            PrintLogging("Adjusting speed, giving aura etc")

            if(not IsRunning?):
                set IsRunning = true
                spawn{StartHealAura()}

    OnHealingCharacterEliminated(EliminationResult:elimination_result):void=
        if(Agent:= EliminationResult.EliminatedCharacter.GetAgent[]):
            RemoveAgentFromHealVFXTracker(Agent)
    
    RemoveAgentFromHealVFXTracker(Agent:agent):void=
        
        if(Index:= AgentVFX.Find[Agent]):
            PrintLogging("Removing VFX on Agent Death")
            HealVFX.End(Agent)

        set AgentVFX = AgentVFX.RemoveAllElements(Agent)

    StartHealAura()<suspends>:void=
        loop:
            if(not IsRunning?):
                break

            TeamCollection:= GetPlayspace().GetTeamCollection()
            if(ActiveSiegeMinions.Length > 0, FirstAgent:=ActiveSiegeMinions[0], Team:=TeamCollection.GetTeam[FirstAgent], TeamMembers:=TeamCollection.GetAgents[Team]):
                
                for(Minion:ActiveSiegeMinions, MinionCharacter:=Minion.GetFortCharacter[]):
                    MinionPos:= MinionCharacter.GetTransform().Translation
                    for(Agent:TeamMembers):
                        if(Agent <> Minion):
                            if(FortCharacter:=Agent.GetFortCharacter[], FortCharacter.GetHealth() < FortCharacter.GetMaxHealth()):

                                if (ThePlayer := player[Agent] or (OnlyHealPlayers = false)):
                                    Dist:=Distance(MinionPos, FortCharacter.GetTransform().Translation), 
                                    if(Dist <= HealthAuraEffectRange):
                                        Health:= Min(FortCharacter.GetHealth()+HealthAuraHealAmount, FortCharacter.GetMaxHealth())
                                        FortCharacter.SetHealth(Health)
                                        FortCharacter.EliminatedEvent().Subscribe(OnHealingCharacterEliminated)
                                        PrintLogging("Healing for {Health}")
                                        HealVFX.SpawnAt(Agent)
                                        HealVFX.Begin(Agent)
                                        set AgentVFX += array{Agent}

            Sleep(HealthAuraHealFrequency)

            TidyVFX()
    
    TidyVFX():void=
        for(Agent:AgentVFX):
            HealVFX.End(Agent)

        set AgentVFX = array{}
