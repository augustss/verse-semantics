using { /Fortnite.com/Characters }
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /Verse.org/Simulation/Tags }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Random }
using { /Fortnite.com/VerseInternalTesting/NexusFramework}


# A Verse-authored creative device that can be placed in a level
ai_handler_device := class(nexus_test_device):

    @editable
    Teleporters : []teleporter_device = array{}

    @editable
    CaptureArea : capture_area_device = capture_area_device{}

    @editable
    NPCs: npc_spawner_device = npc_spawner_device{}

    @editable
    Animals: wildlife_spawner_device = wildlife_spawner_device{}

    #Create array for NPCs, Animals, and Creatures
    var AIArray : []agent = array{}

    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - ai_handler_device")
        
        NPCs.SpawnedEvent.Subscribe(AddAgent)
        Animals.SpawnedEvent.Subscribe(AddAgent)
        CaptureArea.AgentEntersEvent.Subscribe(AgentCaptureEnter)

        (super:)OnBegin() # * All devices will need this line

        AITeleportLoop()
        

    AddAgent(InAgent : agent) : void =
        set AIArray += array{InAgent}
        # Print("Agent Added to Array")

    AgentCaptureEnter(InAgent : agent) : void = 
        Print("Agent Entered Capture Area")

    AITeleportLoop()<suspends> : void =
        loop:
            RandomAIIndex : int = GetRandomInt(0, AIArray.Length - 1)
            if(NewAgent : agent = AIArray[RandomAIIndex]):
                # Teleport with a teleporter device
                RandomTeleporterIndex : int = GetRandomInt(0, Teleporters.Length - 1)
                if(CurrentTeleporter := Teleporters[RandomTeleporterIndex]):
                    CurrentTeleporter.Teleport(NewAgent)
                    Print("Teleporting Agent")
            else:
                Print("Cannot get agent")
            Sleep(4.0)