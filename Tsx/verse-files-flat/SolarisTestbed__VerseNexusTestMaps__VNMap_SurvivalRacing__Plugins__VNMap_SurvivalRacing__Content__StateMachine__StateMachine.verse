using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /Verse.org/Simulation/Tags }
using { /UnrealEngine.com/Temporary/Diagnostics }

using { Interfaces }


StateMachineTag<public> := class(tag){}

BaseStateMachine<public> := class(creative_device, ITickable):

    @editable
    TickTime<private>:float = 0.02

    var Tickables<private>:[]ITickable = array{}

    
    OnBegin<override>()<suspends>:void=
        Print("State Machine Begins")
        # ErrorLogChannel.Print("")

        spawn{ Tick() }


    OnEnd<override>():void=
        Print("State Machine Ends")


    Tick<override>()<suspends>:void=
        loop:
            Sleep(TickTime)
            for(Tickable:Tickables):
                Tickable.Tick()

    
    AddTickable<public>(InTickable:ITickable)<transacts>:void=
        set Tickables += array{InTickable}



