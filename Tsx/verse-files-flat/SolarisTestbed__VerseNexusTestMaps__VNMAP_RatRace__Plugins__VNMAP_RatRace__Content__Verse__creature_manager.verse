
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

# A Verse-authored creative device that can be placed in a level
creature_manager := class(creative_device):
    @editable
    CreatureSpawner:creature_spawner_device = creature_spawner_device {}

    OnCreatureSpawn(Agent:agent):void=
        Print("Creature spawned")

    OnCreatureElim(Result:device_ai_interaction_result):void=
        Print("Creature eliminated")

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - creature_manager")
        if:
            CreatureSpawner.SpawnedEvent.Subscribe(OnCreatureSpawn)
            CreatureSpawner.EliminatedEvent.Subscribe(OnCreatureElim)
            SpawnLimit := CreatureSpawner.GetSpawnLimit()
            #Print("Creature Spawn Limit = {SpawnLimit}")
            #Print("Waiting 1 sec to enable spawner")
            #CreatureSpawner.Enable()
            #Print("Waiting 1 sec to disable spawner and kill creatures")
            #CreatureSpawner.EliminateCreatures()
            #CreatureSpawner.Disable()

            true = false

    OnEnd<override>():void=
        Print("Nexus OnEnd - creature_manager")