using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }


player_elimination_manager := class(creative_device):
    @editable
    XPAwardPlayerKill : int = 100

    @editable
    EliminationMgr:elimination_manager_device = elimination_manager_device{}

    @editable
    ScoreManager:score_manager_device = score_manager_device{}

    OnBegin<override>()<suspends>:void=
        EliminationMgr.EliminationEvent.Subscribe(OnEliminatedPlayer)
        EliminationMgr.EliminatedEvent.Subscribe(OnEliminated)

    OnEliminatedPlayer(Eliminator:?agent):void=
        if (TheEliminator:=Eliminator?):
            ScoreManager.Increment(TheEliminator)

            if (GEM := global_events_accessor{}.Instance[Self]):
                GEM.TriggerEvent(event_type.Player, sub_events.XPEarned, ?Agent:= Eliminator, ?IntArg := option{XPAwardPlayerKill})
                GEM.TriggerEvent(event_type.Player, sub_events.PlayerElminated, ?Agent:= Eliminator)


    OnEliminated(Eliminatee:agent):void=
        if:
            Player := player[Eliminatee]
        then:
            if (GEM := global_events_accessor{}.Instance[Self]):
                GEM.TriggerEvent(event_type.Player, sub_events.PlayerDied, ?Agent:= option{Eliminatee})

