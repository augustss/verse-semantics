using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation/Tags }
using { /Fortnite.com/Characters }
using { /Verse.org/Random }  

creature_vfx_zone_manager:= class (creative_device):
    @editable
    VFX_1:vfx_spawner_device = vfx_spawner_device{}
    @editable
    PlayerCounter_1:player_counter_device = player_counter_device{}
    @editable
    PlayerCounter_2:player_counter_device = player_counter_device{}
    @editable
    PlayerCounter_3:player_counter_device = player_counter_device{}
    @editable
    PlayerCounter_4:player_counter_device = player_counter_device{}
    @editable
    PlayerCounter_5:player_counter_device = player_counter_device{}
    @editable
    CreatureSpawner: creature_spawner_device = creature_spawner_device{}
    @editable
    BossCreature: creature_placer_device = creature_placer_device{}

    var CreatureSpawned: logic = false
    var BossSpawned: logic = false

    OnBegin<override>()<suspends>:void=
        Print ("======Nexus OnBegin creature_vfx_manager======")
        PlayerCounter_1.CountSucceedsEvent.Subscribe(OnPlayerArrived)
        PlayerCounter_1.CountedEvent.Subscribe(OnPlayerCounted)
        PlayerCounter_2.CountSucceedsEvent.Subscribe(OnPlayerArrived)
        PlayerCounter_2.CountedEvent.Subscribe(OnPlayerCounted)
        PlayerCounter_3.CountSucceedsEvent.Subscribe(OnPlayerArrived)
        PlayerCounter_3.CountedEvent.Subscribe(OnPlayerCounted)
        PlayerCounter_4.CountSucceedsEvent.Subscribe(OnPlayerArrived)
        PlayerCounter_4.CountedEvent.Subscribe(OnPlayerCounted)
        PlayerCounter_5.CountSucceedsEvent.Subscribe(OnPlayerArrived)
        PlayerCounter_5.CountedEvent.Subscribe(OnPlayerCounted)
    OnEnd<override>():void=
        Print ("======Nexus OnEnd creature_vfx_manager======")

    OnPlayerArrived():void=
        if (BossSpawned = false):
            set BossSpawned = true
            Print ("Spawning the Boss Creature")
            BossCreature.Spawn()
        else:
            set BossSpawned = false
            Print ("Despawning the Boss Creature")
            BossCreature.Despawn()

        if(CreatureSpawned = false):
            set CreatureSpawned = true
            Print ("Enabling the Creature Spawner")
            CreatureSpawner.Enable()
        else:
            set CreatureSpawned = false
            Print ("Eliminating the Creatures")
            CreatureSpawner.EliminateCreatures()
        spawn:
            StartVFXEffect()
    OnPlayerCounted(Agent:agent):void=
        if (BossSpawned = false):
            set BossSpawned = true
            Print ("Spawning the Boss Creature")
            BossCreature.Spawn()
        else:
            set BossSpawned = false
            Print ("Despawning the Boss Creature")
            BossCreature.Despawn()

        if(CreatureSpawned = false):
            set CreatureSpawned = true
            Print ("Enabling the Creature Spawner")
            CreatureSpawner.Enable()
        else:
            set CreatureSpawned = false
            Print ("Eliminating the Creatures")
            CreatureSpawner.EliminateCreatures()
        spawn:
            StartVFXEffect()
    StartVFXEffect()<suspends>:void=
        Print ("VFX spawned on another thread")
        loop:
            Sleep(0.0)
            VFX_1.Enable()
            Sleep(5.0)
            VFX_1.Disable()