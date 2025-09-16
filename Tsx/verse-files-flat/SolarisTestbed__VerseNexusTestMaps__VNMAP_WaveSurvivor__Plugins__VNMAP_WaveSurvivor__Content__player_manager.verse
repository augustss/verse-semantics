
using { /Fortnite.com/Devices }
using { /Fortnite.com/Characters }
using { /Fortnite.com/Game }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath}

player_manager := class(creative_device):

    @editable
    ItemGranter:item_granter_device := item_granter_device{}
    @editable
    PlayerSpawner:player_spawner_device := player_spawner_device{}
    @editable
    Utils:utils := utils{}

    
    var Character<public>:?fort_character = false
    var alive<internal>:logic := true
    var PlayerLevel<public>:int = 1
    var HealStack<private>:[]float := array{}
    var IsHealing<private>:logic := false

    OnPlayerSpawn<internal>(Agent:agent):void=
        ItemGranter.GrantItem(Agent)
        set Character = option{Agent.GetFortCharacter[]}
        Utils.PlayerSpawnedEvent.Signal()

    EnemyCharDamaged<public>(Result:damage_result):void=
        Amount := Result.Amount
        spawn:
            HealPlayer(Amount)

    OnEliminate<private>(Result:elimination_result):void=
        set alive = false

    PlayerSubscribers<public>():void=
        if(C:=Character?):
            C.EliminatedEvent().Subscribe(OnEliminate)

    HealPlayer<public>(Amount:float)<suspends>:void=
        if(set HealStack = HealStack.Insert[0, array{Amount}]){}
        if:
            IsHealing = false
        then:
            set IsHealing = true
            loop:
                if:
                    Iterations:int = Floor[HealStack[HealStack.Length - 1]]
                    set HealStack = HealStack.RemoveElement[HealStack.Length - 1]
                then:
                    for(X := 1..Iterations):
                        Sleep(0.1)
                        if(C := Character?):
                            C.Heal(1.0)
                if(HealStack.Length < 1){break}
            set IsHealing = false
