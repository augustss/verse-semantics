
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/SceneGraph }

event_type<public> := enum:
    None,
    Base,
    Tower,
    Player

sub_events<public> := enum:
    None,
    BaseUnderAttack,
    TowerUnderAttack,
    TowerUnderAttackTop,
    TowerUnderAttackMid,
    TowerUnderAttackBottom,
    TowerDown,
    TowerDownTop,
    TowerDownMid,
    TowerDownBottom,
    TowerUpgraded,
    TowerUpgradedTop,
    TowerUpgradedMid,
    TowerUpgradedBottom,
    PlayerElminated,
    LevelUp,
    XPEarned,
    QuestComplete,
    Team1SiegeMinionReleased,
    Team2SiegeMinionReleased,
    Team1ShieldsDown,
    Team2ShieldsDown,
    GoldEarned,
    MinionEliminated,
    PlayerDied,
    Win,
    Lose,
    ShieldUnderAttack
 
# need system to unregister from an event
event_system := class():
    var Events<private>:[event_type][]type {__(:sub_events, :?agent, :?int):void} = map{}

    RegisterEvent(EventType:event_type, Callback:type {__(:sub_events, :?agent, :?int):void})<decides><transacts>:void=
        if(not Events[EventType]):
            set Events[EventType] = array{}

        set Events[EventType] += array{Callback}

    TriggerEvent(EventType:event_type, SubEvent:sub_events, ?Agent:?agent = false, ?IntArg:?int = false):void=
        for(k->v : Events[EventType]):
            v(SubEvent, Agent, IntArg)

global_events_accessor_log_channel := class(log_channel){}

# Simulating singleton access to the global_events_manager
global_events_accessor := class():
    Logger:log = log{Channel:= global_events_accessor_log_channel}

    var  GlobalEventsManagerRef<private> : ?global_events_manager = false

    UpdateCachedManagerRef<private>(TaggedActors:generator(creative_object_interface))<transacts>:void=
        GEM := for (TaggedActor :TaggedActors, GlobalEventsManager := global_events_manager[TaggedActor]):
            GlobalEventsManager

        if (GEM.Length > 1):
            Logger.Print("Error! Too many global events manager devices in scene")
        
        set GlobalEventsManagerRef = option{GEM[0]}

    Instance(ContextObject:creative_device)<transacts><decides>:global_events_manager=
        if (GEM := GlobalEventsManagerRef?):
            GEM
        else:
            TaggedActors := ContextObject.FindCreativeObjectsWithTag(global_events_manager_tag{})
            UpdateCachedManagerRef(TaggedActors)
            GlobalEventsManagerRef?

    Instance(ContextObject:creative_device_base)<transacts><decides>:global_events_manager=
        if (GEM := GlobalEventsManagerRef?):
            GEM
        else:
            TaggedActors := ContextObject.FindCreativeObjectsWithTag(global_events_manager_tag{})
            UpdateCachedManagerRef(TaggedActors)
            GlobalEventsManagerRef?

    Instance(ContextObject:entity)<transacts><decides>:global_events_manager=
        if (GEM := GlobalEventsManagerRef?):
            GEM
        else:
            TaggedActors := ContextObject.FindCreativeObjectsWithTag(global_events_manager_tag{})
            UpdateCachedManagerRef(TaggedActors)
            GlobalEventsManagerRef?

global_events_manager := class(creative_device):
    GlobalEventSystem <private>: event_system = event_system{}

    RegisterEvent(EventType:event_type, Callback:type {__(:sub_events, :?agent, :?int):void})<decides><transacts>:void=
        GlobalEventSystem.RegisterEvent[EventType, Callback]

    TriggerEvent(EventType:event_type, SubEvent:sub_events, ?Agent:?agent = false, ?IntArg:?int = false):void=
        GlobalEventSystem.TriggerEvent(EventType, SubEvent, ?Agent:= Agent, ?IntArg := IntArg)