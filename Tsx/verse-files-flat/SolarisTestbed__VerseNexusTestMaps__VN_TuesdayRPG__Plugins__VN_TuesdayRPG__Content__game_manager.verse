using { /Fortnite.com/Devices }
using { /Fortnite.com/FortPlayerUtilities }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Colors }
using { /Verse.org/Simulation }

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

var RoundStatsMap:weak_map(player, rounds_state) = map{}

rounds_state :=class<final><persistable><concrete>():

    HasBeenInitialized:logic=false

    @editable
    TotalRounds:int = 1

    @editable
    RoundsCompleted:int = 1

RoundsStateNewTotalRounds<constructor>(Src: rounds_state,NewValue:int)<transacts> := rounds_state:
    RoundsCompleted:=Src.RoundsCompleted
    TotalRounds:=NewValue

RoundsStateNewRoundsCompleted<constructor>(Src: rounds_state,NewValue:int)<transacts> := rounds_state:
    TotalRounds:=Src.TotalRounds
    RoundsCompleted:=NewValue
    
InitializeRoundState<constructor>()<transacts> := rounds_state:

    HasBeenInitialized:=true
    TotalRounds:= 1
    RoundsCompleted:= 1

ResetRoundState<constructor>()<transacts> := rounds_state:

    HasBeenInitialized:=false
    TotalRounds:= 1
    RoundsCompleted:= 1

# A Verse-authored creative device that can be placed in a level
game_manager := class(creative_device):

    @editable
    AddRoundsButton:button_device=button_device{}

    @editable
    SubtractRoundsButton:button_device=button_device{}

    @editable
    var RoundsState :rounds_state = rounds_state{}

    @editable
    RoundsBillboard:billboard_device = billboard_device{}

    RoundsBillboardMessage<localizes>(RoundsInt:int, TotalRoundsInt:int):message = "Rounds: {RoundsInt}\nTotal Rounds: {TotalRoundsInt}"
    InfiniteGameplayMessage<localizes>(StatusString:string):message = "Infinite Gamplay: {StatusString}"

    @editable
    RoundsDevice:round_settings_device = round_settings_device{}

    @editable
    EndGameDevice:end_game_device = end_game_device{}

    @editable
    RespawnButton:button_device=button_device{}

    @editable
    RespawnVector3:vector3=vector3{}

    @editable
    RespawnRotation:rotation=rotation{}

    @editable
    AutoPlay<public> : logic = false
    
    # Infinite play 

    @editable
    var IsInfiniteGameplay:logic = false # False by default

    @editable
    InfinitePlayBillboard:billboard_device = billboard_device{}

    @editable
    ToggleInfinitePlayButton:button_device=button_device{}

    var PlayerSpawnMap: [player]vector3 = map{}

    @editable
    ItemManager:item_manager=item_manager{}
    
    OnBegin<override>()<suspends>:void=

        Print("Nexus OnBegin - game_manager")

        AddRoundsButton.InteractedWithEvent.Subscribe(OnAddRoundsButtonPress)
        SubtractRoundsButton.InteractedWithEvent.Subscribe(OnSubtractRoundsButtonPress)
        RespawnButton.InteractedWithEvent.Subscribe(OnRespawnButtonPress)
        ToggleInfinitePlayButton.InteractedWithEvent.Subscribe(OnToggleInfinitePlayButtonPress)
        LoadGame()
        UpdateRoundsBillboardText()

        Sleep(0.5)

        # if:
        #     Player:=Self.GetPlayspace().GetPlayers()[0]
        #     not RoundStatsMap[Player]
        #     set RoundStatsMap[Player] = rounds_state{}

    LoadGame():void=

        # Check if the user has 
        if:
            Player:=Self.GetPlayspace().GetPlayers()[0]
            NewRoundState:=RoundStatsMap[Player]
            # NewRoundState.TotalRounds<=NewRoundState.RoundsCompleted
            # set RoundStatsMap[Player] = InitializeRoundState()

        then:
            if(set RoundStatsMap[Player] = InitializeRoundState()){}

        if:
            Player:=Self.GetPlayspace().GetPlayers()[0]
            set RoundsState = RoundStatsMap[Player]
            # not RoundStatsMap[Player]
            # set RoundStatsMap[Player] = rounds_state{}
            # RoundState:=RoundStatsMap[Player]
        then:
            Print("Load successful", ?Color:=NamedColors.Green)
        #     if(RoundsState.HasBeenInitialized?):
        #         set RoundsState = RoundState
                
        #         if:
        #             RoundsState.TotalRounds<=RoundsState.RoundsCompleted
        #             set RoundStatsMap[Player] = ResetRoundState()
        #             NewRoundState := RoundStatsMap[Player]
        #         then:
        #             set RoundsState =  NewRoundState
        #     else:

        #         set RoundsState = InitializeRoundState()
                    

    OnEnd<override>():void=
        # RecordAddedRoundsState()
        Print("Nexus OnEnd - game_manager")

    
    RecordAddedRoundsState():void=
        if:
            Player:=Self.GetPlayspace().GetPlayers()[0]
            set RoundStatsMap[Player] = RoundsStateNewRoundsCompleted(RoundsState, RoundsState.RoundsCompleted+1)
        then:
            Print("Save successful", ?Color:=NamedColors.Green)
        Print("Nexus OnEnd - game_manager")

    OnAddRoundsButtonPress(Agent:agent):void=
        set RoundsState = RoundsStateNewTotalRounds(RoundsState, RoundsState.TotalRounds + 1)
        UpdateRoundsBillboardText()

    OnSubtractRoundsButtonPress(Agent:agent):void=
        set RoundsState = RoundsStateNewTotalRounds(RoundsState, RoundsState.TotalRounds - 1)
        UpdateRoundsBillboardText()
    
    UpdateRoundsBillboardText():void=
        RoundsBillboard.SetText(RoundsBillboardMessage(RoundsState.RoundsCompleted,RoundsState.TotalRounds))

    RoundCompleted(Agent:agent)<suspends>:void=
        # Round device end round
        Sleep(3.0)
        # set RoundsState = RoundsStateNewRoundsCompleted(RoundsState, RoundsState.RoundsCompleted + 1)
        # SaveRoundStateToPlayer1()
        RecordAddedRoundsState()
        Sleep(0.5)
        if(RoundsState.RoundsCompleted>=RoundsState.TotalRounds):
            # END GAME
            # if(set RoundStatsMap[Self.GetPlayspace().GetPlayers()[0]] = ResetRoundState()):
            EndGameDevice.Activate(Agent)
            #  Player:=Self.GetPlayspace().GetPlayers()[0]
        else:
            # END ROUND
            RecordAddedRoundsState()
            RoundsDevice.EndRound(Agent)
            Print("Round should be over")
            loop:
                Sleep(0.0)
                Print("Stress testing end of round")

    SaveRoundStateToPlayer1()<suspends>:void=
        if(set RoundStatsMap[Self.GetPlayspace().GetPlayers()[0]] = RoundsState):
        
    
    RoundCompleted(BigFloat:float)<suspends>:void=
        Print("")

    OnToggleInfinitePlayButtonPress(Agent:agent):void=

        set IsInfiniteGameplay = Not(IsInfiniteGameplay)

        if(IsInfiniteGameplay?):
            InfinitePlayBillboard.SetText(InfiniteGameplayMessage("Enabled"))
        else:
            InfinitePlayBillboard.SetText(InfiniteGameplayMessage("Disabled"))

    OnRespawnButtonPress(Agent:agent):void=

        ZeroRotation:=vector3{X:=0.0,Y:=100.0,Z:=0.0}
        
        if(Player:=player[Agent], SpawnLocation:=PlayerSpawnMap[Player]):

            Player.Respawn(SpawnLocation,rotation{})
        
    # RoundCompletedAsync(Agent:agent)<suspends>:void=
    #     RoundsDevice.EndRound(Agent)
    #     set RoundsCompleted+= 1
    #     if(RoundsCompleted>=TotalRounds):
    #         EndGameDevice.Activate(Agent)


