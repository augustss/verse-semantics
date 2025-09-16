using { /Fortnite.com/Devices }
using { /Fortnite.com/UI}
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
using { /Verse.org/Colors }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /UnrealEngine.com/Temporary/UI }

MakeCountdownTimer<constructor><public>(MaxTime : float, InPlayer : agent) := countdown_timer:
    RemainingTime := MaxTime
    MaybePlayerUI := option{GetPlayerUI[player[InPlayer]]} 

countdown_timer := class:
    # This block runs for each instance of the countdown_timer class.
    # We setup the canvas once here.
    block:
        set Canvas = canvas:
            Slots := array:
                canvas_slot:
                    Anchors := anchors:
                        Minimum := vector2{X := 0.4, Y := 0.4}
                        Maximum := vector2{X := 0.4, Y := 0.4}
                    Offsets := margin{Top := -100.0, Left := 0.0, Bottom := 0.0, Right := 0.0}
                    SizeToContent := true
                    Widget := RemainingTimeTextBlock 

    CountdownEndedEvent<public> : event(float) = event(float){}

    StartCountdown<public>() : void =
        if (PlayerUI := MaybePlayerUI?):
            PlayerUI.AddWidget(Canvas)

            # Update the UI when we start the timer to see the initial RemainingTime on screen
            UpdateUI()
            spawn:
                RunCountdown()

    MaybePlayerUI<internal> : ?player_ui = false
    var RemainingTime<internal> : float = 0.0
    RemainingTimeTextBlock<private> : text_block = text_block{DefaultTextColor := NamedColors.White} 
    RemainingTimeText<localizes><private>(CurrentRemainingTime : int) : message = "{CurrentRemainingTime}"
    var Canvas<private> : canvas = canvas{}
    var TotalTime<private> : float = 0.0
    # The timer "precision": how often, in seconds, it ticks.
    TimerTickPeriod<private> : float = 1.0

    RunCountdown<private>()<suspends> : void =
        # We loop with the TimerTickPeriod.
        # The UI is also updated each time.
        loop:
            Sleep(TimerTickPeriod)
            set TotalTime += TimerTickPeriod
            set RemainingTime -= TimerTickPeriod
            UpdateUI()

            # Timer End
            if (RemainingTime <= 0.0):
                
                Canvas.RemoveWidget(RemainingTimeTextBlock)

                 if (UI := MaybePlayerUI?):
                     UI.RemoveWidget(Canvas)

                CountdownEndedEvent.Signal(TotalTime)
                break
    UpdateUI<private>() : void =
        if (IntTime := Int[RemainingTime]):
            RemainingTimeTextBlock.SetText(RemainingTimeText(IntTime))

countdown_timer_manager := class(creative_device):
    @editable
    EndGame : end_game_device = end_game_device{}

    var CountdownTimer : countdown_timer = countdown_timer{}
    @editable
    InitialCountdownTime : float = 30.0

    OnEnd<override>():void=
        Print ("Nexus OnEnd - time_manager")

        # Finding a player and starting the countdown event if they're found
    OnBegin<override>()<suspends> : void =
        Print ("Nexus OnBegin - time_manager")
        if:
            FirstPlayer := Self.GetPlayspace().GetPlayers()[0]
            PlayerUI := GetPlayerUI[player[FirstPlayer]]
        then:
            set CountdownTimer = countdown_timer{MaybePlayerUI := option{PlayerUI}, RemainingTime := InitialCountdownTime}
            Print ("Timer successfully started")
            CountdownTimer.StartCountdown()

            CountdownTimer.CountdownEndedEvent.Await()
            
            EndGame.Activate(FirstPlayer)
        else:
            Print("Nexus Error - time_manager.OnBegin() | Can't find a player from GetPlaySpace, timer not started")
