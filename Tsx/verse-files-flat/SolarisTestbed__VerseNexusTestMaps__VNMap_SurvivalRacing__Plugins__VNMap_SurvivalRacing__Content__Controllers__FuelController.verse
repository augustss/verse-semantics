
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { GameEvents }
using { Utils }


FuelController := class(creative_device):

    @editable
    GameplayEvents:EventsBus = EventsBus{}

    @editable
    FuelPumps:[]fuel_pump_device = array{}

    @editable
    TimeBeforeRestoreFuelInSecs:float = 100.0


    OnBegin<override>()<suspends>:void=
        Print("Fuel Pumps Controller Starts")
        MakeSubscriptions()

    

    MakeSubscriptions():void=
        Print("Make subscriptions on emptyng pumps")
        for(Fuel : FuelPumps):
            Fuel.EmptyEvent.SubscribeAgentWithExtra(OnEmptyPumpSubscribe, Fuel)
    #         Fuel.EmptyEvent.Subscribe(OnEmptyPumpSubscribe1)


    # OnEmptyPumpSubscribe1(InAgent:agent):void=
    #     Print("")


    OnEmptyPumpSubscribe(InAgent:agent, InPump:fuel_pump_device):void=
        spawn:
            FillUpPump(InPump, TimeBeforeRestoreFuelInSecs)

    
    FillUpPump(InPump:fuel_pump_device, InSecs:float)<suspends>:void=
        Print("Before filling pump sleep for {InSecs} secs")
        Sleep(InSecs)
        Print("Trying to Reset Pump")
        InPump.Reset()



