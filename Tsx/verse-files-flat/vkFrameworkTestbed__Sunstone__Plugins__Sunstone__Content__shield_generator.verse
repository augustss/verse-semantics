
using { /Fortnite.com/Devices }
using { /Fortnite.com/UI }
using { /Fortnite.com/Playspaces }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/UI}
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /UnrealEngine.com}
using { /Verse.org/Colors }
using { /Verse.org/Colors/NamedColors }

shield_generator := class(creative_device):

    @editable
    ShieldGenerators:[]objective_device = array{}

    @editable
    ShieldBarrier:barrier_device = barrier_device{}

    @editable
    GeneratorDestroyedMessage:string = ""
    
    @editable
    ShieldGeneratorMessageEvent:sub_events = sub_events.Team1ShieldsDown

    var Destroyed:int = 0

    OnBegin<override>()<suspends>:void=
        for (ShieldGenerator:ShieldGenerators):
            ShieldGenerator.DestroyedEvent.Subscribe(OnGeneratorDestroyed)

    OnGeneratorDestroyed(Agent:agent):void=
        set Destroyed += 1
        if (Destroyed = ShieldGenerators.Length):
            ShieldBarrier.Disable()
            DisplayGeneratorDestroyedMessage(Agent)

    TextForUI<localizes>(InText:string):message = "{InText}"

    DisplayGeneratorDestroyedMessage(Agent:agent):void=
        if (GEM := global_events_accessor{}.Instance[Self]):
            GEM.TriggerEvent(event_type.Base, ShieldGeneratorMessageEvent, ?Agent := option{Agent})