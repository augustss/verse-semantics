using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

key_game_event:= class<concrete><final>:
    @editable
    SubEventToUse : sub_events = sub_events.None
    @editable
    Message : hud_message_device = hud_message_device{}
    @editable
    AudioStinger : audio_player_device = audio_player_device{}
    @editable
    Trigger : trigger_device = trigger_device{}

# A Verse-authored creative device that can be placed in a level
key_events_listener_device := class<final>(creative_device):
    @editable
    KeyEventsData : []key_game_event = array{}

    var KeyEventsMap : [sub_events]key_game_event = map{}
    var EventsManagerRef : ?global_events_manager = false
  
    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        PopuplateMap()
        Subscriptions()

    Subscriptions():void=
        if (EventsManager := global_events_accessor{}.Instance[Self]):
            set EventsManagerRef = option{EventsManager}
            if:
                EventsManager.RegisterEvent[event_type.Player, HandleEvent]
                EventsManager.RegisterEvent[event_type.Base, HandleEvent]
                EventsManager.RegisterEvent[event_type.Tower, HandleEvent]
            else:
                Print("Key Events: Failed to Register Events")

    HandleEvent(SubEvent: sub_events, Agent : ?agent, IntArgs : ?int):void=
        if:
            KeyEvent := KeyEventsMap[SubEvent]
        then:

            case (SubEvent):
                sub_events.BaseUnderAttack =>
                    TeamCollection :=  Self.GetPlayspace().GetTeamCollection()
                    Teams := TeamCollection.GetTeams()
                    OtherTeam := for (Team : Teams, not TeamCollection.IsOnTeam[Agent?, Team]): 
                        Team
                    if:
                        AttackedAgent := TeamCollection.GetAgents[OtherTeam[0]][0]
                    then:
                        TriggerDevices(KeyEvent, option{AttackedAgent})
                sub_events.ShieldUnderAttack =>
                    TeamCollection :=  Self.GetPlayspace().GetTeamCollection()
                    Teams := TeamCollection.GetTeams()
                    OtherTeam := for (Team : Teams, not TeamCollection.IsOnTeam[Agent?, Team]): 
                        Team
                    if:
                        AttackedAgent := TeamCollection.GetAgents[OtherTeam[0]][0]
                    then:
                        TriggerDevices(KeyEvent, option{AttackedAgent})
                sub_events.XPEarned =>
                        if:
                            XPGained := IntArgs?
                            AgentRef := Agent?
                        then:
                            KeyEvent.Message.Show(AgentRef, XPMessage(XPGained))
                            KeyEvent.AudioStinger.Play(AgentRef)
                            KeyEvent.Trigger.Trigger(AgentRef)

                _ => TriggerDevices(KeyEvent, Agent)
                

    TriggerDevices(KeyEvent :key_game_event, Agent : ?agent):void=
        if (AgentRef := Agent?):
            KeyEvent.Message.Show(AgentRef)
            KeyEvent.AudioStinger.Play(AgentRef)
            KeyEvent.Trigger.Trigger(AgentRef)
        else:
            KeyEvent.Message.Show()
            KeyEvent.AudioStinger.Play()
            KeyEvent.Trigger.Trigger()

    PopuplateMap():void=
        for (Event : KeyEventsData):
            if:
                not KeyEventsMap[Event.SubEventToUse]
                set KeyEventsMap[Event.SubEventToUse] = Event
            else:
                Print("Failed to Populate map for keys event. Potentially duplicate event used in key event array")    

    XPMessage<localizes>(XPGained: int):message = "+{XPGained} XP!"   