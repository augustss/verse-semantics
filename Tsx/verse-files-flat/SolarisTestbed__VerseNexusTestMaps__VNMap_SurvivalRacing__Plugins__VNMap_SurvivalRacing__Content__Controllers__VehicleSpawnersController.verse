
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
using { /Fortnite.com/Vehicles }

using { Utils }
using { GameEvents }
using { Entities }


VehicleSpawnerController := class(creative_device):

    @editable
    GameplayEvents:EventsBus = EventsBus{}

    @editable
    RigVehicle:vehicle_spawner_big_rig_device = vehicle_spawner_big_rig_device{}

    @editable
    ArmoredBusVehicle:vehicle_spawner_armored_battle_bus_device = vehicle_spawner_armored_battle_bus_device{}

    @editable
    DirtbikeVehicle:vehicle_spawner_dirtbike_device = vehicle_spawner_dirtbike_device{}

    @editable
    NitroVehicle:vehicle_spawner_getaway_device = vehicle_spawner_getaway_device{}

    @editable
    SportbikeVehicle:vehicle_spawner_sportbike_device = vehicle_spawner_sportbike_device{}

    @editable
    SportCarVehicle:vehicle_spawner_sports_car_device = vehicle_spawner_sports_car_device{}


    var VehicleSpawners:[]vehicle_spawner_device = array{}



    OnBegin<override>()<suspends>:void=
        Print("Vehicle Spawners Controller Starts")
        CollectVehiclesInArray()
        MakeVehiclesSubscriptions()



    CollectVehiclesInArray():void=
        if:
            set VehicleSpawners = array{vehicle_spawner_device[RigVehicle],
                                        vehicle_spawner_device[ArmoredBusVehicle],
                                        vehicle_spawner_device[DirtbikeVehicle],
                                        vehicle_spawner_device[NitroVehicle],
                                        vehicle_spawner_device[SportbikeVehicle],
                                        vehicle_spawner_device[SportCarVehicle]
                                    }
        then:
            Print("Successfully added all vehicles in array")
        
        else:
            Print("Could not cast vehicles in one array")



    MakeVehiclesSubscriptions():void=
        Print("Make vehicles subscriptions")
        for(Spawner:VehicleSpawners):
            Spawner.AgentEntersVehicleEvent.SubscribeAgentWithExtra(AgentEntersVehicleSubscriber, Spawner)
            Spawner.AgentExitsVehicleEvent.Subscribe(AgentExitsVehicleSubscriber)


    
    AgentEntersVehicleSubscriber(InAgent:agent, InSpawner:vehicle_spawner_device):void=
        Print("Subscriber called on vehicle enters")
        if(Ps := GameplayEvents.DTOs.Players?):
            for(P : Ps):
                if (P.Agent = InAgent):
                    Print("Found player from array in comparing And Aassign Vehicle")
                    set P.Vehicle = option{InAgent.GetFortCharacter[].GetVehicle[]}

    
    AgentExitsVehicleSubscriber(InAgent:agent):void=
        Print("Subscriber called on vehicle exits")
        if(Ps := GameplayEvents.DTOs.Players?):
            for(P : Ps):
                if (P.Agent = InAgent):
                    Print("Found player from array in comparing And Aassign Vehicle")
                    set P.Vehicle = false

        

    # SpawnVehicleOverNSecs(InSecs:float, InSpawner:vehicle_spawner_device)<suspends>:void=
    #     Print("Waiting {InSecs} before spawning new")
    #     Sleep(InSecs)
    #     Print("Respawn vehicle")
    #     InSpawner.RespawnVehicle()


