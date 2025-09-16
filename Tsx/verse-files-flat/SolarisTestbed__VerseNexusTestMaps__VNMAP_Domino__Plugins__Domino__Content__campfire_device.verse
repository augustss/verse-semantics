using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

campfire_fire_device := class(creative_device):

    @editable
    Campfire : campfire_device = campfire_device{}

    var CampfireStatus<private> : logic = false;

    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - campfire_fire_device")        
        Sleep(4.0)
        Campfire.Enable()
        Campfire.AgentEntersEffectAreaEvent.Subscribe(TurnOn)
        Campfire.AgentExitsEffectAreaEvent.Subscribe(TurnOff)
        Campfire.LitEvent.Subscribe(OnLit)
        Campfire.ExtinguishedEvent.Subscribe(OnExtinguished)

    OnEnd<override>():void=
        Print("Nexus OnEnd - campfire_fire_device")

    OnLit(Agent : agent):void=
        if (CampfireStatus = true):
            Print("NexusError - fire shouldn't be litted in OnLit() of campfire_fire_device")
            return
        Print("Campfire litted by agent")
        set CampfireStatus = true

    OnExtinguished(Agent : agent):void=
        if (CampfireStatus = false):
            Print("NexusError - fire should be litted in OnExtinguished() of campfire_fire_device")
            return
        Print("Campfire extinguished by agent")
        set CampfireStatus = false

    TurnOn(Agent : agent):void=
        Campfire.Light(Agent)

    TurnOff(Agent : agent):void=
        Campfire.Extinguish(Agent)    