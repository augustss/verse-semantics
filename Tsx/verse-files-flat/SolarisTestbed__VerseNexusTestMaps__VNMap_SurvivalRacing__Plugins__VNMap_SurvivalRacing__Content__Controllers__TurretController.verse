
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { GameEvents }


TurretController := class(creative_device):

    @editable
    GameplayEvents:EventsBus = EventsBus{}

    @editable
    Turrets:[]automated_turret_device = array{}


    OnBegin<override>()<suspends>:void=
        Print("Turret Controller Starts")
        spawn:
            SubscribeOnRaceStart()

    
    SubscribeOnRaceStart()<suspends>:void=
        Print("Subscribe turrets on Racer Start Event")
        GameplayEvents.RaceStartEvent.Await()
        EnablingAllTurrets()


    EnablingAllTurrets():void=
        Print("Enabling all turrets")
        for(Turret:Turrets):
            Turret.Enable()