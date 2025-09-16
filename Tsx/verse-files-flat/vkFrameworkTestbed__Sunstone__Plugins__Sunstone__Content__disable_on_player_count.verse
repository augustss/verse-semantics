
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

disable_entry := class<concrete>:
    @editable
    GuardSpawners:[]guard_spawner_device = array{}

    @editable
    CreaturePlacers:[]creature_placer_device = array{}

    @editable
    NpcSpawners:[]npc_spawner_device = array{}

    @editable
    CreatureSpawners:[]creature_spawner_device = array{}

    @editable
    DisableIfPlayerCountBelow:int = 0

disable_on_player_count := class(creative_device):

    @editable
    Entries:[]disable_entry = array{}

    OnBegin<override>()<suspends>:void=
        Playspace := GetPlayspace()
        Playspace.PlayerAddedEvent().Subscribe(OnPlayerChange)
        Playspace.PlayerRemovedEvent().Subscribe(OnPlayerChange)

    UpdateDisableList():void=
        Playspace := GetPlayspace()
        Players := Playspace.GetPlayers()
        PlayerCount := Players.Length
        for (Entry:Entries):
            if (Entry.DisableIfPlayerCountBelow > PlayerCount):
                for (GuardSpawner:Entry.GuardSpawners):
                    GuardSpawner.Disable()
                for (CreaturePlacer:Entry.CreaturePlacers):
                    CreaturePlacer.Despawn()
                for (NpcSpawner:Entry.NpcSpawners):
                    NpcSpawner.Disable()
                for (CreatureSpawner:Entry.CreatureSpawners):
                    CreatureSpawner.Disable()
            else:
                for (GuardSpawner:Entry.GuardSpawners):
                    GuardSpawner.Enable()
                for (CreaturePlacer:Entry.CreaturePlacers):
                    CreaturePlacer.Spawn()
                for (NpcSpawner:Entry.NpcSpawners):
                    NpcSpawner.Enable()
                for (CreatureSpawner:Entry.CreatureSpawners):
                    CreatureSpawner.Enable()
    
    OnPlayerChange(Player:player):void=
        UpdateDisableList()