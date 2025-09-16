
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

player_data_table := class<final><persistable>:
    RatSpawns:int = 0

MakePlayerDataTable<constructor>(OldTable:player_data_table)<transacts> := player_data_table:
    RatSpawns := OldTable.RatSpawns

data_type := class<computes><unique><abstract>:
    DebugString():string

DataType := module:
    rat_spawns_data<public> := class<computes><unique>(data_type):
        DebugString<override>():string = "Rat Spawns"

    RatSpawns<public>:rat_spawns_data = rat_spawns_data{}

# A Verse-authored creative device that can be placed in a level
player_data_manager := class(creative_device):

    InitializePlayer(Player:player):void=
        if:
            not PlayerDataMap[Player]
            set PlayerDataMap[Player] = player_data_table{}

    InitializeAllPlayers(Players:[]player):void=
        for(Player:Players):
            InitializePlayer(Player)

    GetPlayerData(Agent:agent)<decides>:player_data_table=
        var PlayerData:player_data_table = player_data_table{}

    RecordPlayerData(Agent:agent, Data:data_type):void=
        if:
            Player := player[Agent]
            PlayerDataTable := PlayerDataMap[Player]
            if(Data = DataType.RatSpawns):
                RatSpawnsData := PlayerDataTable.RatSpawns

                set PlayerDataMap[Player] = player_data_table:
                    MakePlayerDataTable<constructor>(PlayerDataTable)
                    


    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - player_data_manager")

    OnEnd<override>():void=
        Print("Nexus OnEnd - player_data_manager")