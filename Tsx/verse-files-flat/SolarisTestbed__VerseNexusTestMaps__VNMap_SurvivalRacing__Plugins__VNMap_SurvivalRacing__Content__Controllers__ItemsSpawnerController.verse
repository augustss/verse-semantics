
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { Utils }
using { GameEvents }


ItemsSpawnerController := class(creative_device):

    @editable
    GameplayEvents:EventsBus = EventsBus{}

    @editable
    ItemsSpawners:[]item_spawner_device = array{}


    OnBegin<override>()<suspends>:void=
        Print("Items Spawner Controller Starts")
        MakeSubscriptions()

    
    MakeSubscriptions():void=
        for(Spawner:ItemsSpawners):
            Spawner.ItemPickedUpEvent.SubscribeAgentWithExtra(OnItemPickedSubscribe, Spawner)
            Spawner.SetTimeBetweenSpawns(15.0)

    
    OnItemPickedSubscribe(Agent:agent, InSpawner:item_spawner_device):void=
        Print("Item Picked")
        spawn:
            SpawnItemWithin2Sec(InSpawner)


    SpawnItemWithin2Sec(InSpawner:item_spawner_device)<suspends>:void=
        Print("Will Spawn Item over 10 sec")
        Sleep(10.0)
        Print("Spawn Item")
        InSpawner.SpawnItem()


    

    