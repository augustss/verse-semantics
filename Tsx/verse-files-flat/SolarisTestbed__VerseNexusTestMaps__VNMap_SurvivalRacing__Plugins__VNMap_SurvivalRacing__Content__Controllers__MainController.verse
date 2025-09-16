
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }

using { GameEvents }
using { Entities }
using { StateMachine }
using { Utils }


MainController := class(creative_device):

    @editable
    GameplayEvents:EventsBus = EventsBus{}

    var SM:?BaseStateMachine = false

    var Players:[]PlayerWrapper = array{}



    OnBegin<override>()<suspends>:void=
        Print("GCTest: OnBegin")

        if(SMCO:BaseStateMachine = BaseStateMachine[GetCreativeObjectsWithTag(StateMachineTag{})[0]]): 
            Print("Found State Machine")
            set SM = option{SMCO}
        else:
            Print("State Machine is Not Found")

        InitPlayers() 
        


    OnEnd<override>():void=
        Print("GCTest: OnEnd")


    InitPlayers()<suspends>:void=
        Print("Initialize Players")
        players := GetPlayspace().GetPlayers()
        Print("Found [{players.Length}] Players")
        Print("Awaiting RainsVFXs, LightingConfigs, LeavesConfigs")
        LocalRains := GameplayEvents.LocalRainsInitEvent.Await()
        LightingConfigs := GameplayEvents.LightingsInitEvent.Await()
        LeavesConfigs := GameplayEvents.LeavesInitEvent.Await()
        
        Print("Looping players to generate PlayerWrappers")
        for(ind:int=0..players.Length-1):
            if:
                P:=players[ind]
                Ch := P.GetFortCharacter[]
                A := Ch.GetAgent[]
                LR := LocalRains[ind]
                LC := LightingConfigs[ind]
                LvC := LeavesConfigs[ind]
            then:
                PW:=ConstructPlayer(Ch, A, P, LR, LC, LvC)
                spawn { PW.LightingSwitchUpdate() }
                set Players += array{PW}
                if(SM?.AddTickable(PW)){}
                Print("Got Fine Character and Agent from Player")
            else:
                Print("Could Not Get Character and Agent from Player")

        set GameplayEvents.DTOs.Players = option{Players}
        


    
