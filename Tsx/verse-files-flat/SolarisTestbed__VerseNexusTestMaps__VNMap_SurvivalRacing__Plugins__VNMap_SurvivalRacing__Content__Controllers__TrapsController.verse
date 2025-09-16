
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { GameEvents }


TrapsController := class(creative_device):

    @editable
    GameplayEvents:EventsBus = EventsBus{}

    @editable
    Traps:[]trick_tile_device = array{}


    OnBegin<override>()<suspends>:void=
        Print("Traps Controller Starts")
        SubscribeOnRaceStart()


    SubscribeOnRaceStart()<suspends>:void=
        Print("Subscribe traps on Race Start Event")
        GameplayEvents.RaceStartEvent.Await()
        Print("Spawn traps triggering over N secs")
        # spawn:
        #     TriggerTrapsOverSec(2.0)


    TriggerTrapsOverSec(InSec:float)<suspends>:void=
        loop:
            Sleep(InSec)
            Print("Trigger Traps {Traps.Length}")
            for(Trap:Traps):
                Trap.Trigger()
