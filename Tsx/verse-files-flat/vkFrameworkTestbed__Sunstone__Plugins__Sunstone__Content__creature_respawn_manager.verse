
using { /Fortnite.com/Devices }
using { /Fortnite.com/Characters }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

creature_spawn_info := class<concrete>:
    @editable
    CreaturePlacer:creature_placer_device = creature_placer_device{}

    @editable
    RespawnTime: float = 0.0

    @editable
    MaxHealth: float = 100.0

    @editable
    MaxShield: float = 0.0

    OnCreatureEliminated(Eliminator:?agent):void=
        # TODO: Find a way to terminate this task when the device is destroyed
        spawn { StartRespawnTimer() }
    
    OnCreatureSpawned(Creature:agent):void=
        if (FortCharacter:= Creature.GetFortCharacter[]):
            FortCharacter.SetMaxHealth(MaxHealth)
            FortCharacter.SetMaxShield(MaxShield)  

    StartRespawnTimer()<suspends>:void=
        Sleep(RespawnTime)
        CreaturePlacer.Spawn()

creature_respawn_manager := class(creative_device):
    @editable
    CreatureSpawnData:[]creature_spawn_info = array{}

    OnBegin<override>()<suspends>:void=
        InitCreaturePlacers()

        spawn:
            StartInitialSpawnLoop()

    StartInitialSpawnLoop()<suspends>:void=
        Sleep(1.0)
        for (SpawnInfo:CreatureSpawnData):
            SpawnInfo.CreaturePlacer.Spawn()
        
    InitCreaturePlacers():void=
        for (SpawnInfo:CreatureSpawnData):
            SpawnInfo.CreaturePlacer.SpawnedEvent.Subscribe(SpawnInfo.OnCreatureSpawned)
            SpawnInfo.CreaturePlacer.EliminatedEvent.Subscribe(SpawnInfo.OnCreatureEliminated)

        
    