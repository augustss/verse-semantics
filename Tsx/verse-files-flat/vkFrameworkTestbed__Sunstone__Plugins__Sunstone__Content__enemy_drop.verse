using { /Fortnite.com/Devices }
using { /Fortnite.com/Teams }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/SceneGraph }

GoldGrantMesage<localizes>(Agent:agent, Target:agent):message = "Granting gold to {Agent} for eliminating {Target}"
EliminatedMesage<localizes>(HasEliminator:int, HasEliminated:int):message = "Creature eliminated {HasEliminator} by {HasEliminator}"

item_drop_entry := class<concrete>:
    @editable
    XPAwardCreatureKill : int = 10
    @editable
    XPAwardNPCKill : int = 5

    @editable
    Granter:item_granter_device = item_granter_device{}

    @editable
    ScoreManager:score_manager_device = score_manager_device{}

    @editable
    NpcSpawners:[]npc_spawner_device = array{}

    @editable
    CreatureSpawners:[]creature_spawner_device = array{}

    @editable
    CreaturePlacers:[]creature_placer_device = array{}

    @editable
    GuardSpawners:[]guard_spawner_device = array{}

    Initialize():void=
        for (NpcSpawner:NpcSpawners):
            NpcSpawner.EliminatedEvent.Subscribe(OnNpcEliminated)
        for (CreatureSpawner:CreatureSpawners):
            CreatureSpawner.EliminatedEvent.Subscribe(OnNpcEliminated)
        for (CreaturePlacer:CreaturePlacers):
            CreaturePlacer.EliminatedEvent.Subscribe(OnCreatureEliminated)
        for (GuardSpawner:GuardSpawners):
            GuardSpawner.EliminatedEvent.Subscribe(OnNpcEliminated)

    OnCreatureEliminated(Eliminator:?agent):void=
        if (TheEliminator:=Eliminator?):
            Granter.GrantItem(TheEliminator)
            ScoreManager.Increment(TheEliminator)

            if (GEM := global_events_accessor{}.Instance[Granter]):
                GEM.TriggerEvent(event_type.Player, sub_events.XPEarned, ?Agent:= Eliminator, ?IntArg := option{XPAwardCreatureKill})
                GEM.TriggerEvent(event_type.Player, sub_events.MinionEliminated, ?Agent:= option{TheEliminator})

    OnNpcEliminated(Elimination:device_ai_interaction_result):void=
        HasEliminator := if (Elimination.Source?) then 1 else 0 
        HasEliminated := if (Elimination.Target?) then 1 else 0 
        #Print(EliminatedMesage(HasEliminator, HasEliminated))
        if:
            Eliminator := Elimination.Source?
            Eliminated := Elimination.Target?
        then:
            Granter.GrantItem(Eliminator)
            ScoreManager.Increment(Eliminator)

            if (GEM := global_events_accessor{}.Instance[Granter]):
                GEM.TriggerEvent(event_type.Player, sub_events.XPEarned, ?Agent:= option{Eliminator}, ?IntArg := option{XPAwardNPCKill})
                GEM.TriggerEvent(event_type.Player, sub_events.MinionEliminated, ?Agent:= option{Eliminator})
            #Print(GoldGrantMesage(Eliminator, Eliminated))

enemy_item_drop := class(creative_device):
    @editable
    ItemDropEntries:[]item_drop_entry = array{}

    @editable
    TowerDestroyedGranter:item_granter_device = item_granter_device{}

    @editable
    PrintLogging : logic = false


    OnBegin<override>()<suspends>:void=
        for (Entry:ItemDropEntries):
            Entry.Initialize()

        CreateTowerRoutines()

    TowerDestructionRoutine(Tower:entity)<suspends>:logic=
        PlaySpace:= GetPlayspace()
        TeamCollection:= PlaySpace.GetTeamCollection()

        loop:
            if(PrintLogging?){Print("Awaiting Tower Destruction")}
            if(TowerComponent:= Tower.GetComponent[tower_controller_component]):
                TowerComponent.DeactivateEvent.Await()
            
                if(PrintLogging?){Print("Tower Destroyed")}

                for(TeamToGive:TeamCollection.GetTeams()):
                    
                    if:
                        Agents:= TeamCollection.GetAgents[TeamToGive]
                        FirstInTeam:=Agents[0]
                        TeamCollection.GetTeamAttitude[FirstInTeam, Tower] <> team_attitude.Friendly
                    then:
                        TowerDestroyedGranter.GrantItem(FirstInTeam)
                        if(PrintLogging?){Print("Granting Item to the tower's team")}
                    else:
                        if(PrintLogging?){Print("Not granting to this team as it's friendly to the tower")}
            else:
                if(PrintLogging?){Print("ERROR: No tower component on this entity!")}
                break
        return true

    CreateTowerRoutines<private>()<suspends>:void=
        if (SimulationEntity := GetSimulationEntity[]):
            Towers:=SimulationEntity.FindDescendantEntitiesWithComponent(tower_controller_component)
            for(Tower:Towers):
                spawn:
                    TowerDestructionRoutine(Tower)
                

