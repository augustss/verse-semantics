using { /Fortnite.com/Devices }
using { /Fortnite.com/UI}
using { /Verse.org/Simulation }
using { /Fortnite.com/Characters }
using { /Verse.org/Colors }
using { /Verse.org/Simulation/Tags }
using { /Verse.org/Random }
using { /Fortnite.com/Game }

capture_area_tag<public> := class(tag):

capture_area_manager := class (creative_device):

    @editable
    EnterAudioDevice : audio_player_device = audio_player_device {}


    OnBegin<override>()<suspends>:void=
        Print ("Nexus OnBegin - capture_area_manager")
        InitCaptureAreas()

    OnEnd<override>():void=
        Print ("Nexus OnEnd - capture_area_manager")
        
    # Subscribing our Tagged capture_area_devices with OnPlayerEtersArea and calling a SetHeight function on the capture_area_device
    InitCaptureAreas():void=

        CaptureAreas := GetCreativeObjectsWithTag (capture_area_tag{})
        SomeFloatVal := GetRandomFloat (3.0, 9.0)

            for (Obj : CaptureAreas):

                if (CaptureArea := capture_area_device[Obj]):
                    
                    CaptureArea.SetHeight(SomeFloatVal)
                    CaptureArea.AgentEntersEvent.Subscribe(OnPlayerEntersArea)
                else:
                    Print ("NexusError - capture_area_manager.InitCaptureAreas() | A tagged CaptureArea was not successfully passed in an array as a capture_area_device")
# When the agent is referenced in the zone, we play the EnterAudioDevice
    OnPlayerEntersArea(Agent: agent):void=
        Print("OnPlayerEntersArea triggered")
        EnterAudioDevice.Play(Agent)


