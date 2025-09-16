
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }

using { GameEvents }
using { Entities }
using { Systems }


RaceLogicController := class(creative_device):

    @editable
    GameplayEvents:EventsBus = EventsBus{}

    @editable
    RaceManager:race_manager_device = race_manager_device{}

    @editable
    CheckPoints:[]race_checkpoint_device = array{} # Last in array should be final checkpoint

    @editable
    StartBarrier:barrier_device = barrier_device{}

    @editable
    StartRaceZoneFlags:[]creative_prop = array{} # Add two diagonal flags to determine zone where all players have to be to start race

    var Players:[]player = array{}

    var PlayerWrappers:[]PlayerWrapper = array{}

    FarDistanceReset:float = 20000.0



    OnBegin<override>()<suspends>:void=
        Print("Race Logic Controller Starts")
        set Players = GetPlayspace().GetPlayers()

        spawn{ CheckAllPlayersInStartZone() }
        spawn{ SynchronizePlayerWrappersInit() }



    CheckAllPlayersInStartZone()<suspends>:void=
        loop:
            # Print("I'm LOOPING")
            Sleep(0.5)
            if (CheckPlayersTranslation[]):
                GameplayEvents.RaceStartEvent.Signal()
                RaceManager.Begin()
                StartBarrier.Disable()
                break
                      
    
    CheckPlayersTranslation()<decides><transacts>:void=
        for(i:int = 0..Players.Length - 1):
            if(Character:fort_character = Players[i].GetFortCharacter[], StartFlag:creative_prop = StartRaceZoneFlags[0]):
                # Print("{Character.GetTransform().Translation.Y} ?? {StartFlag.GetTransform().Translation.Y}")
                Character.GetTransform().Translation.Y < StartFlag.GetTransform().Translation.Y


    SynchronizePlayerWrappersInit()<suspends>:void=
        Print("Spawn Players Wrappers Sync")
        loop:
            Sleep(0.1)
            if(Pls := GameplayEvents.DTOs.Players?):
                set PlayerWrappers = Pls
                break

        Print("Players Wrappers Synched - Length = [{PlayerWrappers.Length}]")
        for(PW : PlayerWrappers):
            set PW.CheckPoints = option{ CheckPointsSystem{AllCheckPoints := CheckPoints, Player := PW, FarDistanceReset := FarDistanceReset} }
            if (CP:=PW.CheckPoints?) then CP.Init()



        
