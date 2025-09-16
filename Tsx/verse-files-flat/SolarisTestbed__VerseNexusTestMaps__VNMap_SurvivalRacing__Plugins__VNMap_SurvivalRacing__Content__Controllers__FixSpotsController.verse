
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
using { /Fortnite.com/Vehicles }

using { GameEvents }
using { Utils }


FixSpotsController := class(creative_device):

    @editable
    GameplayEvents:EventsBus = EventsBus{}

    @editable
    FixVolumes:[]volume_device = array{}


    OnBegin<override>()<suspends>:void=
        Print("Fix Spots Controller Starts")
        MakeSubscriptions()


    MakeSubscriptions():void=
        Print("Make Fix Volumes subscriptions")
        for(Volume:FixVolumes):
            Print("For each volume subscribe on Player Enter")
            Volume.AgentEntersEvent.Subscribe(OnPlayerEntersVolume)
            Volume.AgentExitsEvent.Subscribe(OnPlayerExitsVolume)

    
    OnPlayerEntersVolume(InAgent:agent):void=
        Print("Agent got in Pit Stop Volume")
        if(Ps := GameplayEvents.DTOs.Players?):
            for(P : Ps):
                if (P.Agent = InAgent):
                    Print("Found player from array in comparing who get in PitStop")
                    if(P.Vehicle?):
                        set P.IsVehicleOnPitStop = true


    OnPlayerExitsVolume(InAgent:agent):void=
        Print("Agent got out Pit Stop Volume")
        if(Ps := GameplayEvents.DTOs.Players?):
            for(P : Ps):
                if (P.Agent = InAgent):
                    Print("Found player from array in comparing who get in PitStop")
                    if(P.Vehicle?):
                        set P.IsVehicleOnPitStop = false


    

        
