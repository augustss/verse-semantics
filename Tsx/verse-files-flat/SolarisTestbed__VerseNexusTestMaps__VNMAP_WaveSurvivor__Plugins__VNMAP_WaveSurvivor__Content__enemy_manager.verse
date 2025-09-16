
using { /Fortnite.com/Devices }
using { /Fortnite.com/Characters }
using { /Verse.org/Simulation }
using { /Verse.org/Random }
using { /UnrealEngine.com/Temporary/Diagnostics }

enemy_manager := class(creative_device):

    @editable
    CreatureSpawners:[]creature_spawner_device := array{}
    @editable
    Powerups:[]health_powerup_device := array{}
    @editable
    ScoreManager:score_manager_device := score_manager_device{}
    @editable
    Hud:hud_device := hud_device{}
    @editable
    PlayerInfo:player_manager := player_manager{}
    
    var Creatures<private>:[]agent := array{}
    var CreaturesToChar<private>:[]fort_character := array{}
    var x<private>:int := 0
    var EliminatedCount<public>:int := 0

    OnEnemySpawn<private>(Agent:agent):void=
        set Creatures += array{Agent}
        for(Creature:Creatures):
            if:
                set CreaturesToChar += array{Creature.GetFortCharacter[]}

    OnEnemyEliminated<private>(Result:device_ai_interaction_result):void=
        set EliminatedCount = EliminatedCount + 1
        Print(ToString(EliminatedCount))
        Hud.SliderAdding()
        if:
            EnemyAgent:agent := Result.Target?
            AgentToChar := EnemyAgent.GetFortCharacter[]
            if(GetRandomInt(0, 99) < 35):
                if:
                    Powerups[x].TeleportTo[AgentToChar.GetTransform()]
                else:
                    set x = x+1
                    Powerups[x].TeleportTo[AgentToChar.GetTransform()]
                    if(x = Powerups.Length - 1):
                        set x = 0

    EnemySubscribers<public>():void=
        for(CreatureSpawner:CreatureSpawners):
            CreatureSpawner.SpawnedEvent.Subscribe(OnEnemySpawn)
            CreatureSpawner.EliminatedEvent.Subscribe(OnEnemyEliminated)
            for(Enemy:CreaturesToChar):
                Enemy.DamagedEvent().Subscribe(PlayerInfo.EnemyCharDamaged)
        