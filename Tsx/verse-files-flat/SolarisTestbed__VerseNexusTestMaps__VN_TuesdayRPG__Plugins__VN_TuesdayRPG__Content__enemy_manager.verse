
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /Verse.org/Simulation/Tags }
using { /Verse.org/Random }
using { /UnrealEngine.com/Temporary/Diagnostics }

enemytag:=class(tag){}

enemymanagertag:=class(tag){}

item_drop := struct<concrete>:

    @editable
    DropPercentage:float = 0.5

    @editable
    ItemName<public>:string = ""
drop_table:= class<concrete>:

    @editable
    Drops : []item_drop = array{}

enemy_spawn_data<public> := class<abstract><computes>:

    @editable
    EnemyName:string = "EnemyName"

    @editable
    MinGoldReward:int = 1

    @editable
    MaxGoldReward:int = 1
        
    OnEnemyKilled<protected>(Agent:agent):void=
        Print("OnEnemyKilled parent class called")

enemy_spawn_class<public>:=class<concrete><final>(enemy_spawn_data):
    
    @editable
    XPReward:int = 15

    @editable
    EnemyManager:creature_manager_device=creature_manager_device{}
    
    @editable
    EnemySpawner:creature_spawner_device=creature_spawner_device{}

    var LocalPlayerManager:player_manager=player_manager{}
    var LocalItemManager:item_manager = item_manager{}
    @editable
    DropTable:drop_table = drop_table{}

    InitializeEnemySpawner()<suspends>:void=
        EnemyXPReward:int = XPReward
        EnemyManager.MatchingCreatureTypeEliminatedEvent.Subscribe(OnEnemyKilled)

    var EnemyID:int = -1
    
    # Polymorphism example
    OnEnemyKilled<override>(Agent:agent):void=
        GainEXP(Agent)
        GainGold(Agent)
        LocalPlayerManager.CheckIfIsQuestEnemy(EnemyID)
        
        for(ItemChance:DropTable.Drops):

            ItemRoll : float = GetRandomFloat(0.0,1.0)

            if(ItemChance.DropPercentage > ItemRoll):

            if(Player:=player[Agent]):

                LocalItemManager.GrantPlayerItem(ItemChance.ItemName, Player)

            else:

                LocalItemManager.GrantPlayerItem(ItemChance.ItemName)


    GainEXP(Agent:agent):void=
        LocalPlayerManager.GainExp(Agent,XPReward)

    GainGold(Agent:agent):void=
        LocalPlayerManager.GainGold(Agent, GetRandomInt(MinGoldReward, MaxGoldReward))



enemy_manager := class(creative_device):

    @editable
    GlobalPlayerManager:player_manager=player_manager{}

    # Enemy Globals
    @editable
    EnemySpawnData:[]enemy_spawn_class=array{}

    @editable
    ItemManager:item_manager = item_manager{}

    @editable
    AutoPlayButton:button_device=button_device{}

    var IsAutoBattleGoing :logic= false

    @editable
    GameManager:game_manager = game_manager{}

    @editable
    AutoEnemyEliminateTime:float = 2.0
    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - enemy_manager")

        set IsAutoBattleGoing = GameManager.AutoPlay 
        AutoPlayButton.InteractedWithEvent.Subscribe(OnAutoPlayButtonPress)
        sync:
            Sleep(1.0)

            Sleep(0.1)
            Print("Tagged enemy spawners found: " + ToString(GetCreativeObjectsWithTag(enemytag{}).Length))
        # Mikita H: Commented due to issue related with EnemySpawner var     
        #sync:
            #Sleep(1.0)
            #EnemySpawners := GetCreativeObjectsWithTag(enemytag{})

            #Sleep(0.1)
            #Print("Tagged enemy spawners found: " + ToString(EnemySpawners.Length))
        sync:
            Sleep(0.3)
            InitializeEnemyData()
            if(GameManager.AutoPlay?):
                Sleep(5.0)
                AutomaticEnemiesEliminated()

    OnEnd<override>():void=
        Print("Nexus OnEnd - enemy_manager")

    InitializeEnemyData()<suspends>:void=
        
        EnemySpawnDataCount:int = EnemySpawnData.Length
        var EnemySpawnIndex:int = 0

        loop:
            if(EnemySpawnClass:enemy_spawn_class=EnemySpawnData[EnemySpawnIndex]):
                set EnemySpawnClass.LocalPlayerManager = GlobalPlayerManager
                set EnemySpawnClass.LocalItemManager = ItemManager
                EnemySpawnClass.InitializeEnemySpawner()
                set EnemySpawnClass.EnemyID = EnemySpawnIndex
            set EnemySpawnIndex += 1

            if(EnemySpawnIndex>=EnemySpawnDataCount):
                break

    EliminateEnemies<public>(EnemiesInt:int):void=
        if(EnemySpawner:=EnemySpawnData[EnemiesInt].EnemySpawner):
            EnemySpawner.EliminateCreatures()
    
    ChangeActiveSpawner(QuestInt:int):void=
        if:
            NewSpawnData := EnemySpawnData[QuestInt].EnemySpawner
        then:
            
            if(OldSpawnData := EnemySpawnData[QuestInt-1].EnemySpawner):
                OldSpawnData.Disable()
                
            NewSpawnData.Enable()

    AutomaticEnemiesEliminated()<suspends>:void=
        # var EnemiesInt:int=GlobalPlayerManager.QuestInt
        loop:
            Sleep(AutoEnemyEliminateTime)
            if(IsAutoBattleGoing = false):
                break
            EliminateEnemies(GlobalPlayerManager.QuestInt)
            # set EnemiesInt +=1


    ClearAllQuestEnemies():void=

        for(Enemy:EnemySpawnData):
            Enemy.EnemySpawner.EliminateCreatures()

    OnAutoPlayButtonPress(Agent:agent):void=
        set IsAutoBattleGoing = ReverseLogic(IsAutoBattleGoing)
        if(IsAutoBattleGoing=true):
            AutoEliminateTask:=spawn{AutomaticEnemiesEliminated()}

    ReverseLogic(LogicToReverse:logic):logic=
        if(LogicToReverse = false):
            return true
        else:
            return false