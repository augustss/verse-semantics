
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }


# Allows for editor events not exposed in verse to emit events into the verse system
event_emitter_tigger_device := class(creative_device):
    @editable
    InputEmitterTrigger : trigger_device = trigger_device{}

    @editable
    EventToEmit : event_type = event_type.None

    @editable
    SubEventToEmit : sub_events = sub_events.None

    @editable
    EventIntArgs : ?int = false

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        InputEmitterTrigger.TriggeredEvent.Subscribe(EmitEvent)

    EmitEvent(Agent : ?agent):void=
        if (GEM := global_events_accessor{}.Instance[Self], EventToEmit <> event_type.None, SubEventToEmit <> sub_events.None):
            GEM.TriggerEvent(EventToEmit, SubEventToEmit, ?Agent:= Agent, ?IntArg := EventIntArgs)