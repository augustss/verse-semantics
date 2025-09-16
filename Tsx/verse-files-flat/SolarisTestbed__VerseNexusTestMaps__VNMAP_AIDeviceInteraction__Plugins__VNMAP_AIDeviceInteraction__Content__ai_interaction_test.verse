
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

# A Verse-authored creative device that can be placed in a level
ai_interaction_test := class(creative_device):

    #AI devices
    @editable
    AIGuard:guard_spawner_device := guard_spawner_device{}
    @editable
    PatrolPath:ai_patrol_path_device := ai_patrol_path_device{}

    #Tested devices
    @editable
    BarrierDevice:barrier_device := barrier_device{}
    @editable
    DamageDevice:damage_volume_device  := damage_volume_device{}
    @editable
    SkydiveDevice:skydive_volume_device := skydive_volume_device{}
    @editable
    MutatorZoneDevice:mutator_zone_device := mutator_zone_device{}
    @editable
    BarrierColorChangingTile:color_changing_tiles_device := color_changing_tiles_device{}

    #Utils
    var AiAgent:?agent := false

    #TestVariables
    var BarrierCheck:logic := false
    var DamageCheck:logic := false
    var SkydiveCheck:logic := false
    var MutatrorCheck:logic := false

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin!")

        #Getting an agent from the AI device
        AIGuard.SpawnedEvent.Subscribe(GetAgentOnSpawn)
        
        #Listeners methods
        BarrierColorChangingTile.ActivatedEvent.Subscribe(BarrierIgnoresAnAgent)
        DamageDevice.AgentEntersEvent.Subscribe(DamageListenToAIAgent)
        SkydiveDevice.AgentEntersEvent.Subscribe(SkydiveDeviceListenToAIAgent)
        MutatorZoneDevice.AgentEntersEvent.Subscribe(MutatorZoneDeviceListenToAIAgent)

    GetAgentOnSpawn(AIAgent:agent):void=
        set AiAgent = option{AIAgent}
        Print("Agent successfully getted")
        BarrierDevice.AddToIgnoreList(AIAgent)
        Print("AIagent added to Barrier ignore list")
        
    BarrierIgnoresAnAgent(AIagent:agent):void=
        set BarrierCheck = true
        Print("Barrier device ignored the agent")

    DamageListenToAIAgent(AIagent:agent):void=
        set DamageCheck = true
        Print("Damage volume device heard the agent")

    SkydiveDeviceListenToAIAgent(AIagent:agent):void=
        set SkydiveCheck = true
        Print("Skydive volume device heard the agent")

    MutatorZoneDeviceListenToAIAgent(AIagent:agent):void=
        set MutatrorCheck = true
        Print("Mutator volume device heard the agent")

        

    