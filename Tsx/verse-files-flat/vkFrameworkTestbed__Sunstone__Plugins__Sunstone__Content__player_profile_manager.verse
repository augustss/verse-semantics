using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

player_classes := enum:
    Infantry,
    Heavy,
    Scout

# data that clears on each session
session_player_data := class<final>:
    PlayerLevel : int = 1
    CurrentXP : int = 0
    GoldMultiplier : float = 1.0
    XPMultiplier : float = 1.0
    CurrentClass : player_classes = player_classes.Infantry

MakeSessionData<constructor>(In:session_player_data)<transacts> :=
    session_player_data:
        PlayerLevel:= In.PlayerLevel
        CurrentXP:= In.CurrentXP
        GoldMultiplier:= In.GoldMultiplier
        XPMultiplier:= In.XPMultiplier
        CurrentClass:= In.CurrentClass

player_profile_manager_log_channel := class(log_channel){}

player_profile_manager := class<final>(creative_device):
    Logger:log = log{Channel:= player_profile_manager_log_channel}

    var EventsManagerRef<private>:?global_events_manager = false

    var PlayerSessionData<private>:[agent]session_player_data = map{}

    PersistentData : persistent_player_data_manager  = persistent_player_data_manager{}

    OnBegin<override>()<suspends>:void=
        Subscriptions()
        InitializePlayers()

    Subscriptions():void=
        Self.GetPlayspace().PlayerAddedEvent().Subscribe(InitializePlayer)

        if (EventsManager := global_events_accessor{}.Instance[Self]):
            set EventsManagerRef = option{EventsManager}
            if:
                EventsManager.RegisterEvent[event_type.Player, HandlePlayerEvent]
                EventsManager.RegisterEvent[event_type.Base, HandleBaseEvent]
                EventsManager.RegisterEvent[event_type.Tower, HandleTowerEvent]
            else:
                Print("Player Profile: Failed to Register Events")

    InitializePlayers():void=
        for (Player : Self.GetPlayspace().GetPlayers()):
            InitializePlayer(Player)

    InitializePlayer(Player : player):void=
        InitialiseSessionData(Player)
        PersistentData.InitializePlayer(Player)

    InitialiseSessionData(Player:player):void=
        if:
            not PlayerSessionData[Player]
            set PlayerSessionData[Player] = MakeSessionData(session_player_data{})

    GetSessionPlayerData(Agent:agent)<decides><transacts>:session_player_data= 
        var SessionData:session_player_data = session_player_data{}

        # Check if player already has data in the map and use that
        if:
            Player := player[Agent]
            Value := PlayerSessionData[Player]
        then:
            set SessionData = Value
        else:
            Logger.Print("Unable to retrieve player session data")

        SessionData

    UpdateSessionPlayerData(Agent:agent, NewData:session_player_data)<decides><transacts>:void= 
        Player := player[Agent]
        set PlayerSessionData[Player] = NewData
                
    GetPersistentPlayerData(Agent:agent):player_stats_info= 
        PersistentData.GetPlayerStatInfo(Agent)
        
    ClearPersistentData(Agent:agent):void=
        PersistentData.ClearPlayerProfile(Agent)

    HandlePlayerEvent(SubEvent: sub_events, Agent : ?agent, IntArg : ?int):void=
        if:
            AgentRef := Agent?
        then:
            case (SubEvent):
                sub_events.PlayerElminated => PersistentData.RecordStat(AgentRef, player_stat_id.EliminationsPlayers, 1)
                sub_events.MinionEliminated => PersistentData.RecordStat(AgentRef, player_stat_id.EliminationsMinions, 1)
                sub_events.PlayerDied => PersistentData.RecordStat(AgentRef, player_stat_id.Deaths, 1)
                sub_events.Win => PersistentData.RecordStat(AgentRef, player_stat_id.WinCount, 1)
                sub_events.Lose => PersistentData.RecordStat(AgentRef, player_stat_id.LoseCount, 1)
                sub_events.GoldEarned =>
                    if:
                        GoldAmount := IntArg?
                    then: 
                        PersistentData.RecordStat(AgentRef, player_stat_id.GoldCollected, GoldAmount)
                _ =>

        
    HandleBaseEvent(SubEvent: sub_events, Agent : ?agent, IntArgs : ?int):void=
        if:
            AgentRef := Agent?
        then:
            case (SubEvent):
                sub_events.Team1ShieldsDown => PersistentData.RecordStat(AgentRef, player_stat_id.ShieldsDeactivated, 1)
                sub_events.Team2ShieldsDown => PersistentData.RecordStat(AgentRef, player_stat_id.ShieldsDeactivated, 1)
                _ =>

    HandleTowerEvent(SubEvent: sub_events, Agent : ?agent, IntArgs : ?int):void=
        if:
            AgentRef := Agent?
        then:
            case (SubEvent):
                sub_events.TowerDown => PersistentData.RecordStat(AgentRef, player_stat_id.TowersDowned, 1)
                sub_events.TowerUpgraded => PersistentData.RecordStat(AgentRef, player_stat_id.TowersUpgraded, 1)
                _ => 
    
    
