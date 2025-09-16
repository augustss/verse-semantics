
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /Fortnite.com/Characters }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Playspaces }
using { /Fortnite.com/Game }
using { /Verse.org/Simulation/Tags }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Random }
using { /Verse.org/Concurrency }

# using { /Fortnite.com/TestDevices }
var PlayerDataMap : weak_map(player, player_data) = map{}

player_level_bb:=class(tag){}

quest_base<public> := class<concrete>:
    @editable
    EnemiesToDefeat<public> : int =  0
    @editable
    var EnemiesDefeated<public> : int = 0
    @editable
    EnemyName<public> :string = "Monsters"

debug_drawer:= class(debug_draw_channel):

    DebugDraw:debug_draw = debug_draw{Channel := debug_drawer}

sound_type := enum{Success, Failure, LevelUp}

can_afford := enum{Yes, No}


player_data<public> := struct<concrete><persistable><computes>:
    MyFloats : []float = array{1.2,1.1}
    @editable 
    XP : int = 0
    
    @editable 
    MaxXP : int = 0

    @editable 
    Level : int = 1

    @editable
    Gold : int = 15

    InventorySaveStrings : []string = array{}

    EquipmentSaveStrings : []string = array{}

# Used as the overall game manager
player_manager := class(creative_device):

    @editable
    GameManager:game_manager = game_manager{}

    DebugDraw:debug_drawer=debug_drawer{}
    # Player globals
    @editable
    PlayerHUD : hud_manager = hud_manager{}

    @editable 
    DefaultMaxXP : int = 100


    # Global reference to all players in the game
    var Players: []player = array{}

    var FortChars: []fort_character = array{}

    var PlayerData<public>: []player_data = array{}

    @editable
    TestButton : button_device = button_device{}

    Buttons:button_device=button_device{}

    # Town Stuff
    @editable
    VillageLevelBillboard:billboard_device = billboard_device{}

    IntMessage<localizes>(Integer:int) : message = "{Integer}"

    LevelBBMessage<localizes>(LevelInt:int) : message = "Lvl. {LevelInt}"

    # Audio 
    @editable
    LevelUpAudioPlayer:audio_player_device=audio_player_device{}

    @editable
    SuccessAudioPlayer:audio_player_device=audio_player_device{}

    @editable
    FailAudioPlayer<public>:audio_player_device=audio_player_device{}

    @editable
    ItemManager:item_manager = item_manager{}
    # QuestInt will index which quest the players are on
    var QuestInt:int = 0

    @editable
    Quests:[]quest_base = array{} 

    var AllQuestsCompleted : logic = false

    var PlayerLevelBillboards:[]billboard_device = array{}

    @editable 
    DebugButton:button_device=button_device{}

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        # DebugDraw.DebugDraw.DrawSphere(vector3{Z:=150.0}, ?DrawDurationPolicy := debug_draw_duration_policy.Persistent
        Print("Nexus OnBegin - player_manager")

        DebugButton.InteractedWithEvent.Subscribe(OnDebugButtonPress)

        InitializeTask:= spawn{InitializePlayerBase()}

        TestButton.InteractedWithEvent.Subscribe(TestFunction)
        
        _PlayerLevelBillboards := GetCreativeObjectsWithTag(player_level_bb{})
        for(Obj : _PlayerLevelBillboards):
            if(Spawner := array{billboard_device[Obj]}):
                set PlayerLevelBillboards += Spawner
        Sleep(0.5)
        PlayerLevelBillboardsFollowPlayers()
        Print("PlayerLevelBillboards count: {PlayerLevelBillboards.Length}")

    OnEnd<override>():void=
        SaveAllPlayers()
        Print("Nexus OnEnd - player_manager")

    OnDebugButtonPress(Agent:agent):void=

        PlayerInt:int = GetPlayerID(Agent)
        if(_Player:=player[Agent], PlayerSave:=PlayerData[PlayerInt]):
            spawn{ItemManager.InitializeSavedPlayerInventoryAndEquipment(_Player, PlayerSave.InventorySaveStrings, PlayerSave.EquipmentSaveStrings)}




    # Initializes global player values
    InitializePlayerBase()<suspends>:void=
        
        defer:
            branch:
                Sleep(0.5)
                UpdateVillageLevelText()
                Print("{Players.Length} Players found")

        set Players = Self.GetPlayspace().GetPlayers()

        Sleep(1.0)
        var PlayerID:int =0
        for(_Player : Players):
            if(FortPlayer := _Player.GetFortCharacter[]):

                # Initialize Player Data
                var NewPlayerData:player_data = player_data{MaxXP:=DefaultMaxXP}

                # Load Game
                if(PlayerSave:=PlayerDataMap[_Player]):
                    set NewPlayerData = PlayerSave
                    Print("LogInv: This is actually loading the player save")
                    ItemManager.InitializeSavedPlayerInventoryAndEquipment(_Player, PlayerSave.InventorySaveStrings, PlayerSave.EquipmentSaveStrings)
                    # ItemManager.InitializeSavedPlayerInventoryAndEquipment(PlayerID, PlayerSave.InventorySaveStrings, PlayerSave.EquipmentSaveStrings)


                var NewPlayerDataArray:[]player_data= array{NewPlayerData}
                set PlayerData += NewPlayerDataArray
                PlayerAgent:agent = _Player

                var NewFortChar:[]fort_character = array{FortPlayer}
                set FortChars += NewFortChar
                Print("Player Added successfully")

                # Initialize Player UI
                PlayerHUD.AddPlayerUI(PlayerAgent, NewPlayerData)
                Print("Player HUD Added successfully")
                FortPlayer.SprintedEvent().Subscribe(PlayerSprint)
                UpdatePlayerBBLevelText(FortChars.Length-1)
                UpdateQuestInfo()

                if (set GameManager.PlayerSpawnMap[_Player] = FortPlayer.GetTransform().Translation) {}
                Sleep(0.5)
                set PlayerID += 1
                
        PlayerLevelBillboardsFollowPlayers()

        Print(ReturnItem("This is a test for parametric type that returns its parameter!"))
        

    SaveAllPlayers():void=
        for(_Player:Players):
            SavePlayer(_Player)

    SavePlayer(_Player:player):void=

        PlayerID : int = GetPlayerID(_Player)
        # ItemManager:item_manager=GetItemManager()

        EquipmentSaveStrings :=ItemManager.GetPlayerEquipmentSaveStrings(_Player)
        InventorySaveStrings := ItemManager.GetPlayerInventorySaveStrings(_Player)
   
            if(NewEquipmentString:=set PlayerData[PlayerID].EquipmentSaveStrings = EquipmentSaveStrings,
            NewInventoryString:=set PlayerData[PlayerID].InventorySaveStrings =InventorySaveStrings):
                    Print("LogInv: Inv data saved")

                    Print("LogTest: Player Data Inventory Save Strings:")
                    for(SaveString:NewInventoryString):
                        Print("LogTest: {SaveString})")
                    Print("LogTest: Player Data Equipment Save Strings:")
                    for(SaveString:NewEquipmentString):
                        Print("LogTest: {SaveString})")

                    Print("LogTest: Item Manager Inventory Save Strings:")
                    for(SaveString:InventorySaveStrings):
                        Print("LogTest: {SaveString})")
                    Print("LogTest: Item Manager Equipment Save Strings:")
                    for(SaveString:EquipmentSaveStrings):
                        Print("LogTest: {SaveString})")

                    if(set PlayerDataMap[_Player] =PlayerData[PlayerID]){}
                        Print("PLAYERDATA SAVE SUCCESSFUL")
            
            
    ReturnItem(ItemToReturn:t where t:type):t = ItemToReturn

    # GetItemManager()<transacts>:item_manager=

    #     var ItemManager :item_manager=item_manager{}
    #     UnInitItemManager := GetCreativeObjectsWithTag(item_manager_tag{})

    #     if(Obj:=UnInitItemManager[0],NewItemManager:=item_manager[Obj]):

    #         return NewItemManager
        
    #     Err("NexusError: GetItemManager()<transacts>:item_manager=");

    #     return ItemManager

    # Function used to get the Player ID without using creative devices
    GetPlayerID(Agent:agent):int=

        # Local Player ID index
        var IDToReturn:int = 0
        
        # Loop through players
        for(_Player : Players):
           
            if(NewPlayer:agent = Players[IDToReturn]):
                
                if(NewPlayer = Agent):
                   
                    Print("Player found: ID = {IDToReturn}")
                    
                    return IDToReturn

            # If the player isn't found, increase the index    
            set IDToReturn += 1

        # Player not found
        Print("Automated player hit, sending to player 0")
        GetRandomInt(0,Players.Length)

    GetPlayerIDWithFortChar(FortChar:fort_character):int=
        # Local Player ID index
        var IDToReturn:int = 0
        
        # Loop through players
        for(_Player : Players):
            if(NewPlayer:agent = Players[IDToReturn]):
                if(FortChar = NewPlayer.GetFortCharacter[]):
                    return IDToReturn

            # If the player isn't found, increase the index    
            set IDToReturn += 1

        # Player not found
        Print("Player not found")
        -1

    TestFunction(Agent:agent):void=
        TestPlayerInt:int = GetPlayerID(Agent)
        Print("Success for player {TestPlayerInt}")
        GainExp(Agent,500)
    
    GainExp(Agent:agent, XPAmount:int):void=
        
        PlayerID:int = GetPlayerID(Agent)

        if(NewXPInt:int = set PlayerData[PlayerID].XP += XPAmount):
            Print("Player {PlayerID} Has {NewXPInt} XP")
            CheckForLevelUp(PlayerID)

    
    GainGold(Agent:agent, GoldAmount:int):void=
        PlayerID:int = GetPlayerID(Agent)

        if(NewGoldInt:int = NewTotalGoldInt:int = set PlayerData[PlayerID].Gold += GoldAmount):
            Print("Player {PlayerID} Has {NewGoldInt} Gold")
            PlayerHUD.UpdateGoldText(PlayerID,NewTotalGoldInt)

            

    
    CheckForLevelUp(PlayerID:int):void=
        if(_PlayerData: player_data =PlayerData[PlayerID]):

            PlayerHUD.UpdateXPText(PlayerID,_PlayerData.XP,_PlayerData.MaxXP)

            if(var NewMaxXP:int=_PlayerData.MaxXP<=_PlayerData.XP):
                # Happens if level up is true
                if(PlayerAgent:=(Players[PlayerID])):
                    PlaySound(sound_type.LevelUp, PlayerAgent)

                    if(NewLevel:int = set PlayerData[PlayerID].Level += 1):
                        PlayerHUD.UpdateLevelText(PlayerID,NewLevel)
                        UpdatePlayerBBLevelText(PlayerID)
                        UpdateVillageLevelText()
                        Print("Level Up! Player {PlayerID} is now Level {NewLevel}")
                        # Set Max XP
                        OldMaxXP:int = NewMaxXP
                        set NewMaxXP = LevelUpXPCurve(NewMaxXP)
                        if(set PlayerData[PlayerID].MaxXP = NewMaxXP):
                            #Set XP
                            if(NewCurrentXP:int = set PlayerData[PlayerID].XP = _PlayerData.XP-OldMaxXP):
                                PlayerHUD.UpdateXPText(PlayerID,NewCurrentXP,NewMaxXP<#_PlayerData.MaxXP#>)
                                PlayerHUD.AddLevelUpUI(PlayerAgent)
                        
                            

    LevelUpXPCurve(_MaxXP:int):int=

        var MaxXPAsFloat:float = _MaxXP*1.25
        set MaxXPAsFloat/=1.1
        set MaxXPAsFloat *= 1.1
        var MaxXPAsInt:int =0

        # Creating a new value to set a value to a failable in a scope is good
        # if(NewMaxXP := Ceil[MaxXPAsFloat]):
        #     set MaxXPAsInt = NewMaxXP
        #     Print("Max XP is {MaxXPAsInt} which can be concatenated with " + ToString(MaxXPAsInt))

        #     return MaxXPAsInt
        
        # Setting a value to a failable in an if check = bad
        if(set MaxXPAsInt = Ceil[MaxXPAsFloat]):

            Print("Max XP is {MaxXPAsInt} which can be concatenated with " + ToString(MaxXPAsInt))

            return MaxXPAsInt

        0 # 

    GetVillageLevelInt():int=
        # VillageLevel is initialized this way so no matter how many players it always starts at 1
        var VillageLevel:int = -PlayerData.Length +1
        for(PlayersData : PlayerData):
            set VillageLevel += PlayersData.Level
    
        VillageLevel

    UpdateVillageLevelText():void=
        VillageLevelBillboard.SetText(IntMessage(GetVillageLevelInt()))

    # Checks if the player can afford something by price
    CanPlayerAffordThis<public>(Agent:agent,ItemPrice:int):logic=
        PlayerID:int = GetPlayerID(Agent)
        if(PlayersGold:int = PlayerData[PlayerID].Gold):
            if(PlayersGold >= ItemPrice):
                PlaySound(sound_type.Success,Agent)
                return true
            
        FailAudioPlayer.Play(Agent)
        return false 

    # Not needed for the game but used for learning purposes
    Less(PlayerMoney:int, Cost:int)<decides> : int =
        PlayerMoney>Cost

    Equal(PlayerMoney:t, Cost:comparable where t:subtype(comparable))<decides> : t =
        PlayerMoney=Cost

    
    HealPlayer<public>(Agent:agent):void=
        PlayerID:int = GetPlayerID(Agent)
        if(PlayerToHeal:=Agent.GetFortCharacter[]):
            PlayerToHeal.SetHealth(10000.0)
                
    IsPlayerAtFullHealth<public>(Agent:agent):logic=

        var IsFull :logic = false
        if(PlayerToHeal:=Agent.GetFortCharacter[]):
            if(PlayerToHeal.GetMaxHealth() <= PlayerToHeal.GetHealth()):
                set IsFull = true
            else:
                set IsFull = false

        IsFull


    PlaySound(SoundType:sound_type, Agent:agent):void=
        case(SoundType):
            sound_type.Success =>
                SuccessAudioPlayer.Play(Agent)
            sound_type.Failure =>
                FailAudioPlayer.Play(Agent)
            sound_type.LevelUp =>
                LevelUpAudioPlayer.Play(Agent)


    CheckIfIsQuestEnemy(EnemyID: int):void=

        if(AllQuestsCompleted = true) {return}

        if(EnemyID = QuestInt):
            if(set Quests[QuestInt].EnemiesDefeated += 1):     
                UpdateQuestInfo()
                CheckIfQuestIsComplete()
                Print("Quest enemy detected")


    CheckIfQuestIsComplete():void=
        if(Quests[QuestInt].EnemiesDefeated >= Quests[QuestInt].EnemiesToDefeat):
            set QuestInt += 1
            UpdateQuestInfo()
            if(QuestInt >= Quests.Length):
                
                PlayerHUD.SetQuestMessagesToCompleted()

                if(GameManager.IsInfiniteGameplay?):
                    
                    spawn{ResetQuests()}

                    return

                set AllQuestsCompleted = true
                if(Player1:=Players[0]){Task1:=spawn{GameManager.RoundCompleted(Player1)}}
                


    ResetQuests()<suspends>:void=

        for(Quest:Quests):
            set Quest.EnemiesDefeated = 0

        EnemyManagers := GetCreativeObjectsWithTag(enemymanagertag{})
        for(Obj : EnemyManagers):
            if{
                EnemyManagerAsArray := array{enemy_manager[Obj]}
                EnemyManager := EnemyManagerAsArray[0]
                FinalEnemySpawner:=EnemyManager.EnemySpawnData[Quests.Length-1]
            }
            then
            {
                # EnemyManager.ClearAllQuestEnemies()
                EnemyManager.ChangeActiveSpawner(0)
                FinalEnemySpawner.EnemySpawner.Disable()
            }
        Sleep(3.0)
        set QuestInt = 0
        UpdateQuestInfo()

 


    UpdateQuestInfo():void=
        if(CurrentEnemyString :=Quests[QuestInt].EnemyName,
        CurrentEnemiesDefeated:=Quests[QuestInt].EnemiesDefeated,
        CurrentEnemiesToDefeat:=Quests[QuestInt].EnemiesToDefeat):
            Print("successfully got through the if statement")
            PlayerHUD.UpdateQuestInfo(CurrentEnemyString,CurrentEnemiesDefeated,CurrentEnemiesToDefeat)

            # Tests references of a creative_device that references this player_manager

            EnemyManagers := GetCreativeObjectsWithTag(enemymanagertag{})
            for(Obj : EnemyManagers):
                if{
                    EnemyManagerAsArray := array{enemy_manager[Obj]}
                    EnemyManager := EnemyManagerAsArray[0]
                }
                then
                {
                    Print("Enemy Manager does exist!")
                    EnemyManager.ChangeActiveSpawner(QuestInt)
                }

    PlayerLevelBillboardsFollowPlayers()<suspends>:void=
        Print("PlayerLevelBillboardsFollowPlayers, Players amount  = {FortChars.Length}")
        FortCharsInt:= FortChars.Length
        Sleep(1.0)
        loop:
            Sleep(0.0)
            for(PlayersInt:= 0..FortCharsInt, FortChar:=FortChars[PlayersInt], PlayersBB :=PlayerLevelBillboards[PlayersInt]):
                    CharPosition :vector3= FortChar.GetTransform().Translation
                    ZeroRotator := MakeRotation(vector3{X:=0.0,Y:=0.0,Z:=0.0},0.0)
                    if(PlayersBB.TeleportTo[CharPosition,ZeroRotator]):



    UpdatePlayerBBLevelText(PlayerID:int):void=
        if(PlayerBB:=PlayerLevelBillboards[PlayerID],
        PlayerLevel:=PlayerData[PlayerID].Level):
            PlayerBB.SetText(LevelBBMessage(PlayerLevel))


    PlayerSprint(CharAndLogic:tuple(fort_character, logic)):void=
        PlayerID:int = GetPlayerIDWithFortChar(CharAndLogic(0))
        Print("PlayerID that is sprinting = {PlayerID}, sprinting = {LogicAsString(CharAndLogic(1))}")
        if(PlayersBB:=PlayerLevelBillboards[PlayerID]):
            if(CharAndLogic(1) = true):
                PlayersBB.HideText()
                Print("Text should be hiding")
            else:
                PlayersBB.ShowText()

    AutomaticTuesdayRPG()<suspends>:void=
        EnemyManagers := GetCreativeObjectsWithTag(enemymanagertag{})

        # Automatically eliminate enemies
            for(Obj : EnemyManagers):
                if{
                    EnemyManagerAsArray := array{enemy_manager[Obj]}
                    EnemyManager := EnemyManagerAsArray[0]
                }
                then
                {
                    loop:
                        Sleep(5.0)
                        EnemyManager.EliminateEnemies(QuestInt)
                }
        
        # Automatically open player inventory and use a random item
                    

          

    LogicAsString(Logic:logic):string=if(Logic=true){ return "true" }else{ return "false"}