
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

# A Verse-authored creative device that can be placed in a level
creature_trap_device := class(obstacle_device):

    @editable
    TimeBeforeEliminatedFloat:float=1.0
    @editable
    CreatureSpawners:[]creature_spawner_device = array{}

    BeginRound<override>()<suspends>:void=
        Print("")
        for(CreatureSpawner:CreatureSpawners):
            CreatureSpawner.SpawnedEvent.Subscribe(EliminateOnSpawned)

    EliminateOnSpawned(Agent:agent):void=
        AsyncEliminateTask:=spawn{AsyncEliminatedOnSpawned()}

    AsyncEliminatedOnSpawned()<suspends>:void=

        Sleep(TimeBeforeEliminatedFloat)
        
        for(CreatureSpawner:CreatureSpawners):
            CreatureSpawner.EliminateCreatures()